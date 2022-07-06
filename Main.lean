/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy
import Lean.Elab.Frontend

open Lean System Alloy

def compileCShim (leanFile : FilePath) (outFile? : Option FilePath) : IO PUnit := do
  let input ← IO.FS.readFile leanFile
  let (env, ok) ← Elab.runFrontend input Options.empty leanFile.toString `main
  if ok then
    if let some outFile := outFile? then
      EmitFileM.run outFile <| C.emitLocalShim env
    else
      EmitStreamM.run (← IO.getStdout) <| C.emitLocalShim env
  else
    throw <| IO.userError s!"file {leanFile} has errors"

def main (args : List String) : IO UInt32 := do
  if let leanFile :: args := args then
    try
      Lean.initSearchPath (← Lean.findSysroot)
      compileCShim leanFile args.head?
      return 0
    catch e =>
      IO.eprintln s!"error: {toString e}"
      return 1
  else
    let appName := (← IO.appPath).fileName.getD "alloy"
    IO.eprintln s!"Usage: {appName} lean-file [out-file]"
    return 1
