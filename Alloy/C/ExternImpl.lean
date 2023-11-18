/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.IR
import Alloy.C.Shim
import Alloy.Util.Syntax
import Alloy.Util.Binder
import Lean.Compiler.NameMangling
import Lean.Linter.UnusedVariables

namespace Alloy.C
open Lean Parser Elab Command

scoped syntax (name := leanExport) "LEAN_EXPORT" : cDeclSpec

def mkParams (fnType : Lean.Expr)
(bvs : Array BinderSyntaxView) (irParams : Array IR.Param)
: TermElabM Params := do
  let mut decls := #[]
  let mut viewIdx := 0
  let mut paramIdx := 0
  let mut fnType := fnType
  for p in irParams do
    let mut bv? := none
    if let .forallE name ty body info := fnType then
      fnType := body
      -- Attempt to match the parameter to a following binder of the same nams
      for h : i in [viewIdx:bvs.size] do
        have := h.upper
        let bv := bvs[i]
        if bv.id.raw.getId = name then
          bv? := some bv
          viewIdx := i + 1
          let kind := Term.kindOfBinderName name
          Meta.withLocalDecl name info ty (kind := kind) fun fvar => do
            Term.addLocalVarInfo bv.id fvar
          break
    if p.ty.isIrrelevant then
      continue -- Lean omits irrelevant parameters for extern constants
    let (id, tyRef) : Ident × Syntax :=
      if let some bv := bv? then
        match bv.id with
        | `($id:ident) => (id, bv.type)
        | _ => (mkIdent <| Name.mkSimple s!"_{paramIdx}", bv.type)
      else
        (mkIdent <| Name.mkSimple s!"_{paramIdx}", .missing)
    let ty ← liftMacroM <| MonadRef.withRef tyRef do
      expandIrParamTypeToC p.borrow p.ty
    decls := decls.push <| ← `(paramDecl| $ty:cTypeSpec $id:ident)
    paramIdx := paramIdx + 1
  if h : viewIdx < bvs.size then
    throwErrorAt (bvs[viewIdx]'h).id "unknown parameter"
  `(params| $[$decls:paramDecl],*)

def setExtern [Monad m] [MonadEnv m] [MonadError m] (name : Name) (sym : String) : m Unit := do
  let env ← getEnv
  if env.getModuleIdxFor? name |>.isSome then
    throwError "declaration is in an imported module"
  if IR.findEnvDecl env name |>.isSome then
    throwError "declaration already has an implementation"
  /-
  The `extern` attribute only supports `afterSet` on constructors
  and projections, so we manually extend this to normal definitions
  lacking an implementation (i.e., `noncomputable` definitions)
  -/
  let env := externAttr.ext.modifyState env fun s => s.insert name
    {arity? := none, entries := [.standard `all sym]}
  match addExtern env name with
  | .ok env => setEnv env
  | .error e => throwError s!"(compiler) {e}"

def elabExternImpl (exTk : Syntax) (sym? : Option StrLit) (id : Ident) (bvs : Array BinderSyntaxView)
(type : Syntax) (body : CompStmt) : CommandElabM Unit := do
  let name ← resolveGlobalConstNoOverloadWithInfo id
  let (cId, extSym) :=
    match sym? with
    | some sym =>
      (mkIdentFrom sym sym.getString, sym.getString)
    | none =>
      let extSym := "_alloy_c_" ++ name.mangle
      (mkIdentFrom id extSym, extSym)
  withRef id <| setExtern name extSym
  let env ← getEnv
  let some info := env.find? name
    | throwErrorAt id "failed to find Lean definition"
  let some decl := IR.findEnvDecl env name
    | throwErrorAt id "failed to find Lean IR definition"
  let ty ← liftMacroM <| MonadRef.withRef type <| expandIrResultTypeToC false decl.resultType
  let params ← liftTermElabM <| mkParams info.type bvs decl.params
  let fn ← MonadRef.withRef Syntax.missing <| `(function|
    LEAN_EXPORT%$exTk $ty:cTypeSpec $cId:ident($params:params) $body:compStmt
  )
  let cmd ← `(set_option linter.unusedVariables false in alloy c section $fn:function end)
  withMacroExpansion (← getRef) cmd <| elabCommand cmd


/--
Implement a Lean definition by external C function
whose definition is provided here. That is:

```
alloy c extern "alloy_foo" impl foo x := {...}
```

is essentially equivalent to

```
attribute [extern "alloy_foo"] foo
alloy c section LEAN_EXPORT uint32_t alloy_foo(uint32_t x) {...}
```
-/
scoped elab (name := externImpl)
"alloy " &"c " exTk:&"extern " sym?:(str)?
&"impl " id:ident bs:Term.binderIdent* " := " body:cStmt : command => do
  let bvs := bs.map fun id => {ref := id, id, type := mkHoleFrom .missing}
  elabExternImpl exTk sym? id bvs .missing (packBody body)

@[unused_variables_ignore_fn]
def ignoreExternImpl : Linter.IgnoreFunction := fun _ stack _ =>
  stack.matches [`null, ``externImpl]

-- User-level ignore functions are broken across files, so we do this instead
initialize Linter.addBuiltinUnusedVariablesIgnoreFn ignoreExternImpl
