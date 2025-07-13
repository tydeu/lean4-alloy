/-
Copyright (c) 2023 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone, Henrik Böving
-/
import Alloy.C.Syntax
import Alloy.C.Translator
import Lean.Compiler.NameMangling

namespace Alloy.C
open Lean Meta Elab Parser Command

/--
Find the Lean IR Type representing a type constant.

Currently, the standard compiler is implemented in C++ and does not expose this
information to Lean code. Thus, to find it, we compile an example definition
producing the type and extract its IR's result type.
-/
def findIRType (constName : Name) : MetaM IR.IRType := do
  let info ← getConstInfo constName
  let levelParams := info.levelParams
  forallTelescope info.type fun as _ => do
  let levels := levelParams.map mkLevelParam
  let type := mkAppN (mkConst constName levels) as
  let name ← mkAuxDeclName constName
  let decl := .opaqueDecl {
    name, levelParams
    type := ← mkForallFVars as <| ← mkArrow type type
    value := ← mkLambdaFVars as <| mkApp (mkConst ``id [← getLevel type]) type
    isUnsafe := false
  }
  try
    let oldEnv ← getEnv
    addAndCompile decl
    let newEnv ← getEnv
    setEnv oldEnv
    let some decl := IR.findEnvDecl newEnv name
      | throwError "could not get IR of definition using {constName}"
    return decl.resultType
  catch e =>
    throwError "could not compile a definition using {constName}: {e.toMessageData}"

/-- A mapping between a Lean constructor and C constant expression. -/
syntax enumCtor := ctor " => " cExpr:1000

/--
Declare an inductive type that maps to and from an enumeration of C values.
For example:

```lean
alloy c enum LeanTy (..) => c_type
| foo => FOO
| bar => BAR
```

With this definition, calling `of_lean<LeanTy>(foo)` in C returns the value `FOO`
(of type `c_type`) and calling `to_lean<LeanTy>(FOO)` returns a `foo` Lean object.
-/
scoped syntax (name := enumCmd) declModifiers
"alloy " &"c " &"enum " declId optDeclSig " => " cSpec+ (" :=" <|> " where")?
  enumCtor+ (ppDedent(ppLine) computedFields)? optDeriving : command

elab_rules : command
| `(enumCmd| $mods alloy c enum
  $declId $sig => $cType* $[where]? $etors* $[$fields?]? $deriv) => do
  let ref ← getRef
  if etors.size = 0 then
      throwErrorAt declId s!"enum is empty; this is not supported"
  let (ctors, cVals) := Array.unzip <| ← etors.mapM  fun
    | `(enumCtor| $ctor => $val) => pure (ctor, val)
    | etor => throwErrorAt etor "ill-formed enum constructor"
  -- Generate Lean Type
  let cmd ← `($mods:declModifiers inductive $declId $sig where $ctors* $[$fields?]? $deriv)
  withMacroExpansion ref cmd <| elabCommand cmd
  let (name, _) := expandDeclIdCore declId
  let declName ← resolveGlobalConstNoOverloadCore name
  -- Determine C representation
  let irType ← liftTermElabM <| withRef declId <| findIRType declName
  let ffiType ← do
    match irType with
    | .uint8 => pure <| mkIdent `uint8_t
    | .uint16 => pure <| mkIdent `uint16_t
    | .uint32 => pure <| mkIdent `uint32_t
    | t => throwErrorAt declId
      s!"enum inductive has IR type {t}; this is not supported; " ++
      s!"the inductive must compile to a primitive type (e.g., uint*)"
  -- Generate Lean <-> C translators
  let toLean := Name.mkSimple <| "_alloy_to_" ++ name.mangle
  let ofLean := Name.mkSimple <| "_alloy_of_" ++ name.mangle
  let ctorIdxs := ctors.size.fold (init := Array.mkEmpty ctors.size) fun n _ xs =>
    xs.push (quote n)
  let cmd ← MonadRef.withRef .missing `(
    alloy c section
    static inline $cType* $(mkIdent ofLean):ident($ffiType:ident v) {
      switch (v) {
        $[
          case $ctorIdxs:num:
            return $cVals;
        ]*
        default:
          lean_panic_fn(lean_box(-1), lean_mk_string("illegal C value"));
          return -1;
      }
    }
    static inline $ffiType $(mkIdent toLean):ident($[$cType:cSpec]* v) {
      switch (v) {
        $[
          case $cVals:constExpr:
            return $ctorIdxs;
        ]*
        default:
          lean_panic_fn(lean_box(-1), lean_mk_string("illegal Lean value"));
          return -1;
      }
    }
    end
  )
  withMacroExpansion ref cmd <| elabCommand cmd
  modifyEnv fun env => translatorExt.insert env declName {toLean, ofLean}
