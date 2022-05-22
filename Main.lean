import Alloy
import Lean.Elab.Frontend

open Lean System Alloy

def compileCShim (leanFile : FilePath) : IO String := do
  let input ← IO.FS.readFile leanFile
  let (env, ok) ← Lean.Elab.runFrontend input Options.empty leanFile.toString `main
  if ok then
    return Alloy.C.emitLocalShim env |>.toString
  else
    throw <| IO.userError s!"file {leanFile} has errors"

def main (args : List String): IO UInt32 := do
  if h : 0 < args.length then
    Lean.initSearchPath (← Lean.findSysroot)
    let file := args.get ⟨0, h⟩
    try
      IO.println <| ← compileCShim file
      return 0
    catch e =>
      IO.eprintln s!"error: {toString e}"
      return 1
  else
    let appName := (← IO.appPath).fileName.getD "extern"
    IO.eprintln s!"Usage: {appName} lean-file"
    return 1
