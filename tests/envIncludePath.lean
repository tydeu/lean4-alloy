import Alloy.C
open scoped Alloy.C

run_cmd Lean.Elab.Command.liftCoreM <|
  Alloy.C.modifyLocalServerConfig (·.addFlag "--language=c")

set_option Alloy.shimDiagnostics.serverOnly false in
alloy c include "includePath.h"
