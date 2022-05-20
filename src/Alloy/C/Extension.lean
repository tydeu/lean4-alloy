import Lean.Environment

open Lean

namespace Alloy.C

initialize implExt : MapDeclarationExtension Syntax ←
  mkMapDeclarationExtension `Alloy.C.impl

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
