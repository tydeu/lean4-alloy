/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Server.Requests
import Alloy.Util.Server.Worker
import Alloy.Util.Server.Extra

open Lean Server Lsp

namespace Alloy

/--
Wait at most `timeout` for a `Promise` to resolve.
If the timeout is reached or the promise is drooped, return `none`.
Otherwise, return the resolved value.
-/
def waitCore? (p : IO.Promise α) (timeout : UInt32) : BaseIO (Option α) := do
  let result? ← IO.Promise.new
  discard <| BaseIO.mapTask result?.resolve p.result?
  discard <| IO.asTask do IO.sleep timeout; result?.resolve none
  IO.wait <| result?.resultD none

/--
Wait at most `timeout` for a `Promise` to resolve, or, of `timeout` is 0,
wait indefinitely.

If the timeout is reached or the promise is drooped, return `none`.
Otherwise, return the resolved value.
-/
def wait? (p : IO.Promise α) (timeout : UInt32) : BaseIO (Option α) := do
  if timeout > 0 then
    waitCore? p timeout
  else
    IO.wait p.result?

def nullPath : System.FilePath :=
  if System.Platform.isWindows then
    "/nul"
  else
    "/dev/null"

def nullUri : DocumentUri :=
  s!"file://{nullPath}"

def isNullUri (uri : DocumentUri) : Bool :=
  if System.Platform.isWindows then
    uri.endsWith "nul"
  else
    uri = "file:///dev/null"

/-- Like `Lean.Server.chainLspRequestHandler`, but uses `LsCall`. -/
@[macro_inline] def chainLspRequestHandler
  (method : String) [LsCall method α β] [FromJson α] [ToJson β]
  (handler : α → RequestTask β → RequestM (RequestTask β))
: IO Unit :=
  Lean.Server.chainLspRequestHandler method α β handler

class LsLeanState (method : String) (σ : outParam $ Type u)
  extends TypeName σ

--instance : LsLeanState "textDocument/semanticTokens/full" SemanticTokensState := {}

/--
Chains a new statless handler to a stateful LSP request handler. Uses `LsCall`.

An adaption of `Lean.Server.chainLspRequestHandler` & `Lean.Server.chainStatefulLspRequestHandler`.
-/
@[macro_inline] def chainPureStatelessLspRequestHandler
  (method : String) [LsCall method α β] [LsLeanState method σ]
  [FileSource α] [FromJson α] [ToJson β]
  (handler : α → β → RequestM β)
: IO Unit := do
  Lean.Server.chainStatefulLspRequestHandler method α β σ
    (fun p r s => do
      let b ← handler p r.response
      return ({r with response := b}, s))
    (fun _ => pure ())

/--
Chains a new statless handler to a stateful LSP request handler. Uses `LsCall`.

An adaption of `Lean.Server.chainLspRequestHandler` for stateful handlers.
-/
def chainStatelessLspRequestHandler
  (method : String) [LsCall method α β] [FileSource α] [FromJson α] [ToJson β]
  (handler : α → RequestTask β → RequestM (RequestTask β))
: IO Unit := do
  if !(← Lean.initializing) then
    throw <| IO.userError s!"\
      Failed to chain LSP request handler for '{method}': \
      only possible during initialization"
  if let some oldHandler := (← statefulRequestHandlers.get).find? method then
    let handle := fun j => do
      let t ← oldHandler.handle j
      let complete ← IO.mkRef false
      let t ← RequestM.mapTaskCheap t fun
        | .ok r => do
          complete.set r.isComplete
          match FromJson.fromJson? r.response with
          | .ok b => return b
          | .error e => throw <| .internalError s!"\
            Failed to parse original LSP response for `{method}` when chaining: {e}"
        | .error e => throw e
      let params ← liftExcept <| parseRequestParams α j
      let t ← handler params t
      RequestM.mapTaskCheap t fun
        | .ok b => return {response := ToJson.toJson b, isComplete := ← complete.get}
        | .error e => throw e
    statefulRequestHandlers.modify fun rhs =>
      rhs.insert method {oldHandler with handle}
  else
    throw <| IO.userError s!"\
      Failed to chain LSP request handler for '{method}': \
      no initial handler registered"
