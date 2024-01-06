import Alloy.C
open Alloy.C

open Lean Elab Command in
#eval show CommandElabM _ from do
  let leanFile ← IO.FS.realPath <| System.FilePath.mk (← getFileName)
  let includeDir := leanFile.parent.bind (·.parent) |>.get!
  logInfo includeDir.toString
  addServerIncludePath includeDir

set_option Alloy.shimDiagnostics.serverOnly false in
alloy c include "includePath.h"
