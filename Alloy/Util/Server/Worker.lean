/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Data.Lsp.Communication
import Alloy.Util.Server.Methods

open Lean hiding Message
open Lean.Lsp Lean.JsonRpc
open IO (Promise)

namespace Alloy

instance [ToString α] : ToString (ResponseError α) where
  toString
    | {id, code, message, data?} =>
      let data := data?.map (s!"\n{·}") |>.getD ""
      s!"Request {id} failed: {message} (code: {toJson code}){data}"

/-- A `IO.Process.StdioConfig` with all streams set to `piped`. -/
abbrev pipedStdioConfig : IO.Process.StdioConfig :=
  {stdin := .piped, stdout := .piped, stderr := .piped}

/-- State for an `LsWorker`. -/
structure LsState where
  nextID : Nat := 0
  responseMap : RBMap RequestID (Promise (Except (ResponseError Json) Json)) compare := {}
  error? : Option IO.Error := none

/-- A function to handle a LSP notification. -/
abbrev NotificationHandler :=
  Option Json.Structured → BaseIO Unit

/-- Construct a JSON notification handler from a well-typed client handler. -/
@[inline] def mkClientNotificationHandler
  [FromJson α] (method : String)
  [LsClientNote method α] (handle : α → BaseIO Unit)
: NotificationHandler := fun params? => do
  match fromJson? <| toJson params? with
  | .ok v => handle v
  | .error e => return panic! s!"ill-formatted '{method}' notification: {e}"

/-- A method-handler map. -/
abbrev NotificationHandlerMap :=
  RBMap String NotificationHandler compare

namespace NotificationHandlerMap

@[inline] def empty : NotificationHandlerMap :=
  RBMap.empty

@[inline] def toRBMap (self : NotificationHandlerMap) : RBMap String NotificationHandler compare :=
  self

@[inline] def insert (self : NotificationHandlerMap)
  [FromJson α] (method : String) [LsClientNote method α] (handle : α → BaseIO Unit) :=
    self.toRBMap.insert method <| mkClientNotificationHandler method handle

end NotificationHandlerMap

deriving instance Inhabited for Json.Structured

/-- A running language server process. -/
structure LsWorker where
  child : IO.Process.Child pipedStdioConfig
  state : IO.Mutex LsState
  notificationHandlers : IO.Mutex NotificationHandlerMap
  capabilities : ServerCapabilities := {}
  info? : Option ServerInfo := none

namespace LsWorker

/-- The language server's standard input stream. -/
def stdin (self : LsWorker) : IO.FS.Stream :=
  IO.FS.Stream.ofHandle self.child.stdin

/-- Issue the LSP notification `method` with `param`. -/
def notify (self : LsWorker) (method : String) [LsServerNote method α] (param : α) : IO Unit := do
  self.stdin.writeLspNotification {method, param}

/-- Set handler function for a client notifications of `method`. -/
def setNotificationHandler (self : LsWorker) [FromJson α]
(method : String) [LsClientNote method α] (handle : α → BaseIO Unit) : BaseIO Unit := do
  self.notificationHandlers.atomically (·.modify (·.insert method handle))

/-- Execute `act` with the client notification handler `handle` set. -/
def withNotificationHandler [Monad m] [MonadLiftT BaseIO m] [MonadFinally m]
(self : LsWorker) (method : String) [LsClientNote method α] (handle : α → BaseIO Unit) (act : m β) : m β := do
  self.setNotificationHandler method handle
  try
    act
  finally
    self.notificationHandlers.atomically (m := BaseIO) (·.modify (·.erase method))

/--
Open the LSP text document specified by `uri`, `text`, `languageId` and `version`,
try to execute `act` and then finally close the document.
-/
def withTextDocument [Monad m] [MonadLiftT IO m] [MonadFinally m] (self : LsWorker)
(uri : DocumentUri) (text languageId : String) (act : m α) (version := 0) : m α := do
  self.notify "textDocument/didOpen" ⟨{uri, version, text, languageId}⟩
  try
    act
  finally
    self.notify "textDocument/didClose" ⟨⟨uri⟩⟩

/-- Invoke the request/response LSP `method` with `param` and return the response asynchronously. -/
def call (self : LsWorker) (method : String) [LsCall method α β] (param : α) : IO (Task (Except (ResponseError Json) β)) := do
  let (id, p) ← self.state.atomically fun ref => do
    let s ← ref.get
    if let some e := s.error? then
      throw <| IO.userError <| s!"Language server error: {e}"
    let id := s.nextID
    self.stdin.writeLspRequest {id, method, param}
    let p ← Promise.new
    ref.set {s with nextID := id + 1, responseMap := s.responseMap.insert id p}
    return (id, p)
  BaseIO.mapTask (t := p.result) fun r => do
    self.state.atomically (·.modify fun s => {s with responseMap := s.responseMap.erase id})
    return match r with
    | .ok r =>
      match fromJson? r with
      | .ok v => .ok v
      | .error e =>
        .error {
          id, code := .internalError, data? := r
          message := s!"Ill-typed response for request: {e}"
        }
    | .error e => .error e

/--
Read all LSP messages from `stream`, completing requests from `state`.
TODO: Handle request messages.
-/
partial def readLspMessages (stream : IO.FS.Stream) (state : IO.Mutex LsState)
(notificationHandlers : IO.Mutex NotificationHandlerMap) : BaseIO Unit := do
  match (← stream.readLspMessage.toBaseIO) with
  | .ok msg =>
    match msg with
    | .request _id _method _params? =>
      pure ()
    | .notification method params? =>
      if let some handle ← notificationHandlers.atomically (·.get <&> (·.find? method)) then
        handle params?
    | .response id result =>
      if let some p ← state.atomically (·.get <&> (·.responseMap.find? id)) then
        p.resolve <| .ok result
    | .responseError id code message data? =>
      if let some p ← state.atomically (·.get <&> (·.responseMap.find? id)) then
        p.resolve <| .error {id, code, message, data?}
    readLspMessages stream state notificationHandlers
  | .error e =>
    state.atomically fun ref => do
      let s ← ref.get
      for (id, p) in s.responseMap do
        p.resolve <| .error <| ResponseError.mk id .internalError
          s!"Language server terminated without responding to request: {e}" none
      ref.set {s with error? := e}

/--
Pipe lines from the input stream `i` to the output stream `o`
until the input stream `i` closes / encounters an error.
Filters out lines for which `filter` returns `true`.
-/
partial def pipeLines (i o : IO.FS.Stream) (filter : String → Bool := fun _ => false) : BaseIO Unit := do
  let pipeLine := do
    let line ← i.getLine
    unless filter line do
      o.putStr line
  if let .ok _ ← pipeLine.toBaseIO then pipeLines i o filter

/--
Spawn the worker process and initialize the language server.
Filters out error lines for which `errorFilter` returns `true`.
-/
def init (cmd : String) (args : Array String := #[]) (params : InitializeParams) (errorFilter : String → Bool := fun _ => false): IO LsWorker := do
  let child ← IO.Process.spawn {cmd, args, toStdioConfig := pipedStdioConfig}
  discard <| BaseIO.asTask <|
    pipeLines (IO.FS.Stream.ofHandle child.stderr) (← IO.getStderr) errorFilter
  let state ← IO.Mutex.new {}
  let notificationHandlers ← IO.Mutex.new {}
  discard <| BaseIO.asTask <|
    readLspMessages (IO.FS.Stream.ofHandle child.stdout) state notificationHandlers
  let ls : LsWorker := {child, state, notificationHandlers}
  let (⟨capabilities, info?⟩) ← IO.ofExcept
    <| ← IO.wait <| ← ls.call "initialize" params
  ls.notify "initialized" InitializedParams.mk
  return {ls with capabilities, info?}

/-- Exit the language server. -/
def exit (self : LsWorker) : IO Unit := do
  discard <| IO.wait <| ← self.call "shutdown" Json.null
  self.notify "exit" Json.null
