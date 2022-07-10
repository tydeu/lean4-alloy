/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Environment

open Lean
namespace Alloy

/-- Unsafe implementation of `findOrRegisterPersistentExtension`. -/
unsafe def unsafeFindOrRegisterPersistentExtension {α β σ : Type} [Inhabited σ]
(name : Name) (register : Name → IO (PersistentEnvExtension α β σ)) : IO (PersistentEnvExtension α β σ) := do
  let pExts ← persistentEnvExtensionsRef.get
  match pExts.find? (·.name == name) with
  | some ext => unsafeCast ext
  | none => register name

/--
Find the already registered persistent extension with the given name
or register one using the provided function.

We need this because otherwise persistent extensions will encounter a clash
between their precompiled (via `precompileModules`) version loaded as a shared
library (via `--load-dynlib`) and their interpreted version imported via olean.
-/
@[implementedBy unsafeFindOrRegisterPersistentExtension]
opaque findOrRegisterPersistentExtension {α β σ : Type} [Inhabited σ]
(name : Name) (register : Name → IO (PersistentEnvExtension α β σ)) : IO (PersistentEnvExtension α β σ)

/-- Persistent environment extension for storing a single serializable value per module. -/
def ModuleEnvExtension (σ : Type) := PersistentEnvExtension σ σ σ

def registerModuleEnvExtension [Inhabited σ] (mkInitial : IO σ) (name : Name) : IO (ModuleEnvExtension σ) :=
  registerPersistentEnvExtension {
    name            := name
    mkInitial       := pure default
    addImportedFn   := fun _ _ => mkInitial
    addEntryFn      := fun s _ => s
    exportEntriesFn := fun s => #[s]
  }

namespace ModuleEnvExtension

instance [Inhabited σ] : Inhabited (ModuleEnvExtension σ) :=
  inferInstanceAs (Inhabited (PersistentEnvExtension ..))

def find? [Inhabited σ] (ext : ModuleEnvExtension σ) (env : Environment) (mod : Name) : Option σ :=
  env.getModuleIdx? mod >>= fun idx => (ext.getModuleEntries env idx)[0]?

end ModuleEnvExtension
