/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Server.Requests
import Alloy.Util.Server.Worker

open Lean Server Lsp

namespace Alloy

def nullUri : DocumentUri :=
  if System.Platform.isWindows then
    "file:///nul"
  else
    "file:///dev/null"

/-- Like `Lean.Server.chainLspRequestHandler`, but uses `LsCall`. -/
@[macro_inline] def chainLspRequestHandler
(method : String) [LsCall method α β] [FromJson α] [ToJson β]
(handler : α → RequestTask β → RequestM (RequestTask β)) : IO Unit :=
  Lean.Server.chainLspRequestHandler method α β handler
