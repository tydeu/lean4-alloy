/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lake
open Lake DSL System

package alloy

--------------------------------------------------------------------------------
-- # Alloy Build
--------------------------------------------------------------------------------

lean_lib Alloy

@[defaultTarget]
lean_exe alloy {
  root := `Main
  supportInterpreter := true
}

--------------------------------------------------------------------------------
-- # Module Facets
--------------------------------------------------------------------------------

module_facet alloy.c : FilePath := fun mod => do
  let some alloy ← findLeanExe? &`alloy
    | error "no alloy executable configuration found in workspace"
  let exeTarget ← alloy.exe.recBuild
  let modTarget ← mod.leanBin.recBuild
  let cFile := mod.irPath "alloy.c"
  let task ← show SchedulerM _ from do
    exeTarget.bindAsync fun exeFile exeTrace => do
    modTarget.bindSync fun _ modTrace => do
      let depTrace := exeTrace.mix modTrace
      buildFileUnlessUpToDate cFile depTrace do
        proc {
          cmd := exeFile.toString
          args := #[mod.leanFile.toString, cFile.toString]
          env := #[("LEAN_PATH", (← getLeanPath).toString)]
        }
  return ActiveTarget.mk cFile  task

module_facet alloy.c.o : FilePath := fun mod => do
  let oFile := mod.irPath "alloy.c.o"
  let cTarget ← recBuild <| mod.facet &`alloy.c
  let args := #["-I", (← getLeanIncludeDir).toString] ++ mod.leancArgs
  oFileTarget oFile (Target.active cTarget) args "cc" |>.activate
