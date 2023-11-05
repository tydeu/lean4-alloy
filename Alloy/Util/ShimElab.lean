/-
Copyright (c) 2023 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.Util.Shim
import Alloy.Util.Extension
import Alloy.Util.Command
import Alloy.Util.Syntax
import Lean.Elab.AuxDef
import Lean.Elab.Syntax

open Lean Elab Command Parser

namespace Alloy

abbrev ShimElabM := CommandElabM
abbrev ShimElab := Syntax → ShimElabM ShimSyntax

unsafe def mkShimElabAttributeUnsafe (ref : Name) : IO (KeyedDeclsAttribute ShimElab) :=
  mkElabAttribute ShimElab `builtin_shim_elab `shim_elab Name.anonymous `Alloy.ShimElab "macro" ref

@[implemented_by mkShimElabAttributeUnsafe]
opaque mkShimElabAttribute (ref : Name) : IO (KeyedDeclsAttribute ShimElab)

initialize shimElabAttribute : KeyedDeclsAttribute ShimElab ←
  mkShimElabAttribute decl_name%

scoped elab_rules : command
| `($[$doc?:docComment]? $(attrs?)? $attrKind:attrKind elab_rules (kind := $kind) $[: $cat?]? $alts:matchAlt*) => do
  let k := mkIdentFrom kind <| ← resolveSyntaxKind kind.getId
  if let some `shim := cat?.map (·.getId) then
    let attrName := mkIdent `shim_elab
    let attr ← `(Term.attrInstance| $attrKind:attrKind $attrName:ident $k:ident)
    let attrs := expandAttrs attrs? |>.push attr
    let stx ← `($[$doc?:docComment]? @[$attrs,*]
      aux_def elabRules k : Alloy.ShimElab :=
      fun $alts:matchAlt* | _ => no_error_if_unused% throwUnsupportedSyntax)
    withMacroExpansion (← getRef) stx <| elabCommand stx
  else
    throwUnsupportedSyntax

/-- TUse `f` to handle elaboration on the macro expanded syntax. -/
partial def elabSyntaxWith (stx : Syntax) (f : Syntax → ShimElabM α) : ShimElabM α :=
  go stx
where
  go stx :=
    withRef stx <| withIncRecDepth <| withFreshMacroScope do
      withTraceNode `ShimElab.step (fun _ => return stx) do
        liftCoreM <| checkMaxHeartbeats "elaborator"
        if let some (decl, stxNew?) ← liftMacroM <| expandMacroImpl? (← getEnv) stx then
          withInfoTreeContext (mkInfoTree := mkCommandElabInfoTree decl stx) do
            let stxNew ← liftMacroM <| liftExcept stxNew?
            withMacroExpansion stx stxNew do
              go stxNew
        else
          f stx

/-- Adapted from the private `elabCommandUsing` definition in `Lean.Elab.Command`. -/
def elabSyntaxUsing? (stx : Syntax) : List (KeyedDeclsAttribute.AttributeEntry (Syntax → CommandElabM α)) → CommandElabM (Option α)
| [] => return none
| (elabFn::elabFns) => do
  let s ← get
  catchInternalId unsupportedSyntaxExceptionId
    (withInfoTreeContext (mkInfoTree := mkCommandElabInfoTree elabFn.declName stx) do
      elabFn.value stx)
    (fun _ => do set s; elabSyntaxUsing? stx elabFns)

structure ShimInfoValue where
  shimRange : String.Range
  deriving TypeName

@[inline] def mkShimInfo (ref : Syntax) (shimRange : String.Range) : Info :=
  .ofCustomInfo <| .mk ref <| .mk <| ShimInfoValue.mk shimRange

@[inline] def toShimInfo? (info : Info) : Option (Syntax × String.Range) := do
  let .ofCustomInfo {stx, value} := info | failure
  let {shimRange} ← value.get? ShimInfoValue
  return (stx, shimRange)

@[inline] def getShimPos [MonadEnv m] [Monad m] (ext : ModuleEnvExtension Shim) : m String.Pos := do
  return ext.getState (← getEnv) |>.text.source.endPos

def addShimLeaf (ext : ModuleEnvExtension Shim) (stx : Syntax)  (code : String) : ShimElabM PUnit := do
  let startPos ← getShimPos ext
  modifyEnv (ext.modifyState · (·.addCodeSnippet code))
  let endPos ← getShimPos ext
  pushInfoLeaf <| mkShimInfo stx ⟨startPos, endPos⟩

/-- Elaborate some shim code and return the produced syntax. -/
partial def elabShimSyntaxCore (ext : ModuleEnvExtension Shim) (stx : Syntax) : ShimElabM ShimSyntax :=
  elabSyntaxWith stx fun
  | .atom info val => do
    let code := reprintLeaf val info
    addShimLeaf ext stx code
    return stx
  | .ident info rawVal _ _ => do
    let code := reprintLeaf rawVal.toString info
    addShimLeaf ext stx code
    return stx
  | .node _ kind args => do
    let elabFns := shimElabAttribute.getEntries (← getEnv) kind
    if let some r ← elabSyntaxUsing? stx elabFns then
      return r
    let startPos ← getShimPos ext
    -- let mkTree trees := do
    --   let endPos ← getShimPos ext
    --   return .node (mkShimInfo stx ⟨startPos, endPos⟩) trees
    -- withInfoTreeContext (mkInfoTree := mkTree) do
    let args ←
      if kind = choiceKind then id do
        let some arg0 := args[0]?
          | throwError "empty choice node"
        let arg0 ← elabShimSyntaxCore ext arg0
        let shim := ext.getState (← getEnv) |>.text.source
        let shim0 := shim.extract startPos shim.endPos
        let mut args' := #[arg0]
        for arg in args[1:] do
          let arg' ← withoutModifyingEnv <| elabShimSyntaxCore ext arg
          let shim := ext.getState (← getEnv) |>.text.source
          let shim' := shim.extract startPos shim.endPos
          args' := args'.push arg'
          if shim0 ≠ shim' then
            throwError "choice node did not produce the same shim on each elaboration"
        return args'
      else
        args.mapM fun arg => elabShimSyntaxCore ext arg
    let endPos ← getShimPos ext
    return Syntax.node (.synthetic startPos endPos) kind args
  | .missing =>
    throwError s!"shim syntax '{stx.getKind}' lacks a custom elaborator and could not be reprinted"
