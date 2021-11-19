import Lean.Environment

open Lean

namespace Alloy.C

builtin_initialize implExt : MapDeclarationExtension Syntax ←
  mkMapDeclarationExtension `Alloy.C.impl

def implExt.getLocalEntries (env : Environment) : Array (Name × Syntax) :=
  implExt.getState env |>.fold (init := #[]) fun a n s => a.push (n, s)

builtin_initialize includeExt : EnvExtension (Array String) ←
  registerEnvExtension (pure #[])

def includeExt.addEntry (env : Environment) (include : String) : Environment :=
  includeExt.modifyState env fun arr => arr.push include

def includeExt.getEntries (env : Environment) : Array String :=
  includeExt.getState env
