/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.Util.Shim
import Alloy.Util.ShimElab
import Alloy.C.Grammar

open Lean

namespace Alloy.C
open Lean Elab Command

/-! ## Shim Extension -/

initialize shimExt : ModuleEnvExtension Shim ←
  registerModuleEnvExtension (pure {})

/-- Returns the C shim associated with the current module. -/
@[inline] def getLocalShim (env : Environment) : Shim :=
  shimExt.getState env

/-- Returns the C shim associated with the named module (or an empty one if none). -/
def getModuleShim (env : Environment) (mod : Name) : Shim :=
  shimExt.find? env mod |>.getD {}

/--
`#print_c_shim [<mod>]` outputs the C shim of the specified module or the
current C shim of the current module if none is specified. Useful for debugging.
-/
elab outTk:"#print_c_shim" mod?:(ppSpace ident)? : command => do
  match mod? with
  | none => logInfoAt outTk <| toString <| C.getLocalShim (← getEnv)
  | some mod => logInfoAt outTk <| toString <| C.getModuleShim (← getEnv) mod.getId

/-! ## Shim Elaboration -/

/-- Elaborate some shim code at the end of the C shim. -/
@[inline] def elabShimSyntax (stx : Syntax) : ShimElabM ShimSyntax := do
  elabShimSyntaxCore shimExt stx

/-- Reprint a command and add it verbatim to the module's C shim. -/
def addCommandToShim [Monad m] [MonadEnv m] [MonadError m] (cmd : Syntax) : m Unit := do
  let env ← getEnv
  let shim := shimExt.getState env
  if let some shim := shim.pushCmd? cmd then
    setEnv <| shimExt.setState env shim
  else
    throwError s!"command '{cmd.getKind}' could not reprinted and add raw to the C shim"

/--
Elaborate a C command. The steps are as follows:
1. Unpack null nodes and expand macros.
2. Attempt to find and apply a standard Lean `command` elaborator.
3. Otherwise, visit each node of the syntax, expanding macros along the way.
4. For each node, attempt to find and apply an Alloy elaborator for its kind.
If successful, elaborate the node into a `ShimSyntax` and then add it to the shim.
6. For nodes lacking any elaborator or raw tokens, attempt to reprint
the syntax (via `Alloy.reprint`) and add it verbatim to the shim.
-/
def elabShimCommand (cmd : Syntax) : CommandElabM Unit :=
  elabEachCommand cmd fun cmd => do
  let elabFns := commandElabAttribute.getEntries (← getEnv) cmd.getKind
  unless (← elabCommandUsing cmd elabFns) do
    let stx ← elabShimSyntax cmd
    modifyEnv (shimExt.modifyState · (·.addCmd stx))

/--
A section of C code to elaborate.
See `elabShimConmand` for details on the elaboration process.
-/
scoped elab (name := sectionCmd)
"alloy " &"c " &"section" ppLine cmds:cCmd+ ppLine "end" : command => do
  cmds.forM elabShimCommand

/-! ## Extra Utilities -/

/--
Include the provided C header files in the module's shim.
A convenience macro to create multiple `#include` directives at once.
-/
scoped macro (name := includeCmd)
"alloy " &"c " &"include " hdrs:header+ : command => do
  let cmds ← MonadRef.withRef Syntax.missing <|
    hdrs.mapM fun hdr => `(cCmd|#include $hdr)
  `(alloy c section $cmds* end)
