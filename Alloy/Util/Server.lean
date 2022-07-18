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

/-- Like `chainLspRequestHandler` but without the `IO.initializing` check. -/
def forceChainLspRequestHandler (method : String)
    paramType [FromJson paramType]
    respType [FromJson respType] [ToJson respType]
    (handler : paramType → RequestTask respType → RequestM (RequestTask respType)) : IO Unit := do
  if let some oldHandler ← lookupLspRequestHandler method then
    let handle := fun j => do
      let t ← oldHandler.handle j
      let t := t.map fun x => x.bind fun j => FromJson.fromJson? j |>.mapError fun e =>
        IO.userError s!"Failed to parse original LSP response for `{method}` when chaining: {e}"
      let params ← liftExcept <| parseRequestParams paramType j
      let t ← handler params t
      pure <| t.map <| Except.map ToJson.toJson

    requestHandlers.modify fun rhs => rhs.insert method {oldHandler with handle}
  else
    throw <| IO.userError s!"Failed to chain LSP request handler for '{method}': no initial handler registered"
