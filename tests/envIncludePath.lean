import Alloy.C
open scoped Alloy.C

#eval Alloy.C.modifyLocalServerConfig (·.addFlag "--language=c")

set_option Alloy.shimDiagnostics.serverOnly false in
alloy c include "includePath.h"
