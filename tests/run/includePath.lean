import Alloy.C
open Alloy.C

open Lean Elab Command in
run_cmd
  let leanFile ← IO.FS.realPath <| System.FilePath.mk (← getFileName)
  let includeDir := leanFile.parent.bind (·.parent) |>.get!
  logInfo includeDir.toString
  liftCoreM <| modifyLocalServerConfig (·.addIncludePath includeDir)

set_option Alloy.shimDiagnostics.serverOnly false in
alloy c include "includePath.h"
