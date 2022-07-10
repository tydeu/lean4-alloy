/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy

open Lean System Alloy

def compileCShim (module : Name) (outFile? : Option FilePath) : IO PUnit := do
  let env ← Lean.importModules [{module}] Options.empty
  if let some outFile := outFile? then
    EmitFileM.run outFile <| C.emitModuleShim env module
  else
    EmitStreamM.run (← IO.getStdout) <| C.emitModuleShim env module

def main (args : List String) : IO UInt32 := do
  if let module :: args := args then
    try
      Lean.initSearchPath (← Lean.findSysroot)
      compileCShim module.toName args.head?
      return 0
    catch e =>
      IO.eprintln s!"error: {toString e}"
      return 1
  else
    let appName := (← IO.appPath).fileName.getD "alloy"
    IO.eprintln s!"Usage: {appName} lean-file [out-file]"
    return 1
