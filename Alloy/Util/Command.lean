/-
Copyright (c) 2019 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Leonardo de Moura, Mac Malone
-/
import Lean.Elab.Command

namespace Alloy
open Lean Elab Command

/-- Create an info tree node recording the elaboration of `stx` by `elaborator`. -/
@[inline] def mkElabInfoTree (elaborator : Name) (stx : Syntax) (trees : PersistentArray InfoTree) : InfoTree :=
  .node (.ofCommandInfo { elaborator, stx }) trees

/-- Create context info for the info tree from the current state of command elaboration.  -/
def mkCommandElabContextInfo : CommandElabM ContextInfo := do
  let ctx ← read; let s ← get; let scope := s.scopes.head!
  return  {
    env := s.env, fileMap := ctx.fileMap, mctx := {}, currNamespace := scope.currNamespace,
    openDecls := scope.openDecls, options := scope.opts, ngen := s.ngen
  }

/-- Create an info tree for an elaborator with the current command elaboration context. -/
@[inline] def mkCommandElabInfoTree (elaborator : Name) (stx : Syntax) (trees : PersistentArray InfoTree) : CommandElabM InfoTree := do
  return .context (← mkCommandElabContextInfo) (mkElabInfoTree elaborator stx trees)

/-- Adapted from the private `elabCommandUsing` definition in `Lean.Elab.Command`. -/
def elabCommandUsing (stx : Syntax) : List (KeyedDeclsAttribute.AttributeEntry CommandElab) → CommandElabM Bool
| [] => return false
| (elabFn::elabFns) => do
  let s ← get
  catchInternalId unsupportedSyntaxExceptionId
    (withInfoTreeContext (mkInfoTree := mkCommandElabInfoTree elabFn.declName stx) do
      elabFn.value stx *> pure true)
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
            withInfoTreeContext (mkInfoTree := mkCommandElabInfoTree decl stx) do
              let stxNew ← liftMacroM <| liftExcept stxNew?
              withMacroExpansion stx stxNew do
                go stxNew
          else
            f stx
      | _ =>
        withInfoTreeContext (mkInfoTree := mkCommandElabInfoTree `no_elab stx) <|
          throwError "unexpected command"
