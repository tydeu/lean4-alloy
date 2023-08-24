/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Environment

open Lean
namespace Alloy

/-- Persistent environment extension for storing a single serializable value per module. -/
def ModuleEnvExtension (σ : Type) := PersistentEnvExtension σ σ σ

def registerModuleEnvExtension [Inhabited σ] (mkInitial : IO σ)
(name : Name := by exact decl_name%) : IO (ModuleEnvExtension σ) :=
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
