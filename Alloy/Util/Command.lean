/-
Copyright (c) 2019 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Leonardo de Moura, Mac Malone
-/
import Lean.Elab.Command

namespace Alloy
open Lean Elab Command

/-- Private definition from `Lean.Elab.Command`. -/
def mkInfoTree (elaborator : Name) (stx : Syntax) (trees : PersistentArray InfoTree) : CommandElabM InfoTree := do
  let ctx ← read
  let s ← get
  let scope := s.scopes.head!
  let tree := InfoTree.node (Info.ofCommandInfo { elaborator, stx }) trees
  return InfoTree.context {
    env := s.env, fileMap := ctx.fileMap, mctx := {}, currNamespace := scope.currNamespace,
    openDecls := scope.openDecls, options := scope.opts, ngen := s.ngen
  } tree

/-- Adapted from the private `elabCommandUsing` definition in `Lean.Elab.Command`. -/
def elabCommandUsing (stx : Syntax) : List (KeyedDeclsAttribute.AttributeEntry CommandElab) → CommandElabM Bool
| [] => return false
| (elabFn::elabFns) => do
  let s ← get
  catchInternalId unsupportedSyntaxExceptionId
    (withInfoTreeContext (mkInfoTree := mkInfoTree elabFn.declName stx) <| elabFn.value stx *> pure true)
    (fun _ => do set s; elabCommandUsing stx elabFns)

/-- Like `elabCommand` but using a custom function `f` to handle elaboration on the expanded commands. -/
partial def elabEachCommand (stx : Syntax) (f : Syntax → CommandElabM Unit) : CommandElabM Unit :=
  go stx
where
  go stx :=
    withLogging <| withRef stx <| withIncRecDepth <| withFreshMacroScope do
      match stx with
      | Syntax.node _ k args =>
        if k == nullKind then
          -- list of commands => elaborate in order
          -- The parser will only ever return a single command at a time, but syntax quotations can return multiple ones
          args.forM go
        else withTraceNode `Elab.command (fun _ => return stx) do
          if let some (decl, stxNew?) ← liftMacroM <| expandMacroImpl? (← getEnv) stx then
            withInfoTreeContext (mkInfoTree := mkInfoTree decl stx) do
              let stxNew ← liftMacroM <| liftExcept stxNew?
              withMacroExpansion stx stxNew do
                go stxNew
          else
            f stx
      | _ =>
        withInfoTreeContext (mkInfoTree := mkInfoTree `no_elab stx) <|
          throwError "unexpected command"
