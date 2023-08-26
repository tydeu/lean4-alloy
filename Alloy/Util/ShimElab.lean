/-
Copyright (c) 2023 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.Util.Shim
import Alloy.Util.Command
import Alloy.Util.Syntax
import Lean.Elab.AuxDef
import Lean.Elab.Syntax

open Lean Elab Command Parser

namespace Alloy

abbrev ShimElabM := CommandElabM
abbrev ShimElab := Syntax → ShimElabM ShimElem

unsafe def mkAlloyElabAttributeUnsafe (ref : Name) : IO (KeyedDeclsAttribute ShimElab) :=
  mkElabAttribute ShimElab `builtin_shim_elab `shim_elab Name.anonymous `Alloy.ShimElab "macro" ref

@[implemented_by mkAlloyElabAttributeUnsafe]
opaque mkAlloyElabAttribute (ref : Name) : IO (KeyedDeclsAttribute ShimElab)

initialize elabAttribute : KeyedDeclsAttribute ShimElab ←
  mkAlloyElabAttribute decl_name%

elab_rules : command
| `($[$doc?:docComment]? $(attrs?)? $attrKind:attrKind elab_rules (kind := $kind) $[: $cat?]? $[<= $expty?]? $alts:matchAlt*) => do
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
          withInfoTreeContext (mkInfoTree := mkInfoTree decl stx) do
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
    (withInfoTreeContext (mkInfoTree := mkInfoTree elabFn.declName stx) <| elabFn.value stx)
    (fun _ => do set s; elabSyntaxUsing? stx elabFns)

/-- Elaborate some shim code and return the produced syntax. -/
partial def elabShimSyntaxCore (stx : Syntax) (startPos : String.Pos) : ShimElabM ShimElem :=
  elabSyntaxWith stx fun
  | .atom info val =>
    return (reprintLeaf val info, stx)
  | .ident info rawVal _ _ =>
    return (reprintLeaf rawVal.toString info, stx)
  | .node _ kind args => do
    let elabFns := elabAttribute.getEntries (← getEnv) kind
    if let some r ← elabSyntaxUsing? stx elabFns then
      return r
    let mut shim := ""
    let mut args' := #[]
    if kind = choiceKind then
      let some arg0 := args[0]?
        | throwError "empty choice node"
      let (shim0, arg0) ← elabShimSyntaxCore arg0 startPos
      args' := args'.push arg0
      for arg in args[1:] do
        let (shim', arg') ← elabShimSyntaxCore arg startPos
        args' := args'.push arg'
        if shim0 ≠ shim' then
          throwError "choice node did not produce the same shim on each elaboration"
      shim := shim0
    else
      for arg in args do
        let pos := startPos + shim.endPos
        let (shim', arg') ← elabShimSyntaxCore arg pos
        args' := args'.push arg'
        shim := shim ++ shim'
    return (shim, Syntax.node (.synthetic startPos (startPos + shim.endPos)) kind args')
  | .missing =>
    throwError s!"shim syntax '{stx.getKind}' lacks a custom elaborator and could not be reprinted"
