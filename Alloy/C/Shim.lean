/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.Util.Shim
import Alloy.Util.ShimElab

open Lean

namespace Alloy.C

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

/-- Reprint a command and add it verbatim to the module's C shim. -/
def addCommandToShim [Monad m] [MonadEnv m] [MonadError m] (cmd : Syntax) : m Unit := do
  let env ← getEnv
  let shim := shimExt.getState env
  if let some shim := shim.pushCmd? cmd then
    setEnv <| shimExt.setState env shim
  else
    throwError s!"command '{cmd.getKind}' could not reprinted and add raw to the C shim"

/-- Elaborate some shim code at the end of the C shim. -/
@[inline] def elabShimSyntax (stx : Syntax) : ShimElabM ShimSyntax := do
  elabShimSyntaxCore shimExt stx
