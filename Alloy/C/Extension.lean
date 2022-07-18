/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Shim
import Alloy.Util.Extension

open Lean

namespace Alloy.C

initialize implExt : MapDeclarationExtension Function ←
  findOrRegisterPersistentExtension `Alloy.C.impl mkMapDeclarationExtension

initialize shimExt : ModuleEnvExtension Shim ←
  findOrRegisterPersistentExtension `Alloy.C.shim <|
    registerModuleEnvExtension (pure {})

@[inline] def getLocalShim (env : Environment) : Shim :=
  shimExt.getState env

def getModuleShim (env : Environment) (mod : Name) : Shim :=
  shimExt.find? env mod |>.getD {}
