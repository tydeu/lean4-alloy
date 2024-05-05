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

@[default_target]
lean_exe alloy where
  root := `Main
  supportInterpreter := true

--------------------------------------------------------------------------------
-- # Module Facets
--------------------------------------------------------------------------------

module_facet alloy.c mod : FilePath := do
  let exeJob ← alloy.fetch
  let modJob ← mod.olean.fetch
  let cFile := mod.irPath "alloy.c"
  exeJob.bindAsync fun exeFile exeTrace => do
  modJob.bindSync fun _ modTrace => do
    let depTrace := exeTrace.mix modTrace
    let trace ← buildFileUnlessUpToDate cFile depTrace do
      proc {
        cmd := exeFile.toString
        args := #[mod.name.toString, cFile.toString]
        env := #[("LEAN_PATH", (← getLeanPath).toString)]
      }
    return (cFile, trace)

@[inline] def buildAlloyCO (mod : Module) (shouldExport : Bool) : FetchM (BuildJob FilePath) := do
  let oFile := mod.irPath s!"alloy.c.o.{if shouldExport then "export" else "noexport"}"
  let cJob ← fetch <| mod.facet `alloy.c
  let weakArgs := #["-I", (← getLeanIncludeDir).toString] ++ mod.weakLeancArgs
  let cc := (← IO.getEnv "CC").getD "cc"
  let leancArgs := if shouldExport then mod.leancArgs.push "-DLEAN_EXPORTING" else mod.leancArgs
  buildO oFile cJob weakArgs leancArgs cc

module_facet alloy.c.o.export mod : FilePath := buildAlloyCO mod true
module_facet alloy.c.o.noexport mod : FilePath :=  buildAlloyCO mod false
