/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Environment

open Lean

namespace Alloy.C

unsafe def unsafeFindOrRegisterPersistentExt {α β σ : Type} [Inhabited σ]
(name : Name) (register : Name → IO (PersistentEnvExtension α β σ)) : IO (PersistentEnvExtension α β σ) := do
  let pExts ← persistentEnvExtensionsRef.get
  match pExts.find? (·.name == name) with
  | some ext => unsafeCast ext
  | none => register name

@[implementedBy unsafeFindOrRegisterPersistentExt]
opaque findOrRegisterPersistentExt {α β σ : Type} [Inhabited σ]
(name : Name) (register : Name → IO (PersistentEnvExtension α β σ)) : IO (PersistentEnvExtension α β σ)

initialize implExt : MapDeclarationExtension Syntax ←
  -- Extension will clash with `precompileModules` version without this check
  findOrRegisterPersistentExt `Alloy.C.impl mkMapDeclarationExtension

def implExt.getLocalEntries (env : Environment) : Array (Name × Syntax) :=
  implExt.getState env |>.fold (init := #[]) fun a n s => a.push (n, s)

initialize cmdExt : EnvExtension (Array Syntax) ←
  registerEnvExtension (pure #[])

def cmdExt.addEntry (env : Environment) (cmd : Syntax) : Environment :=
  cmdExt.modifyState env fun arr => arr.push cmd

def cmdExt.addEntries (env : Environment) (cmds : Array Syntax) : Environment :=
  cmdExt.modifyState env fun arr => arr.append cmds

def cmdExt.getEntries (env : Environment) : Array Syntax :=
  cmdExt.getState env
