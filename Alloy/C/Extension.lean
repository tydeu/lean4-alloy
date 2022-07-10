/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Syntax
import Alloy.Util.Extension

open Lean

namespace Alloy.C

initialize implExt : MapDeclarationExtension Function ←
  findOrRegisterPersistentExtension `Alloy.C.impl mkMapDeclarationExtension

initialize cmdExt : ModuleEnvExtension (Array Cmd) ←
  findOrRegisterPersistentExtension `Alloy.C.cmds <|
    registerModuleEnvExtension (pure #[])

/-- A C shim -- a plain `Array` of C commands (`Cmd`). -/
abbrev Shim := Array Cmd

@[inline] def getLocalShim (env : Environment) : Shim :=
  cmdExt.getState env

def getModuleShim (env : Environment) (mod : Name) : Shim :=
  cmdExt.find? env mod |>.getD #[]
