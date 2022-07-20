/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Server.AsyncList
import Lean.Data.Lsp.Communication
import Alloy.Util.Server.Initialize

open Lean Lsp JsonRpc

namespace Alloy

/-- Like `Lean.Lsp.Ipc.ipcStdioConfig`, but an `abbrev`. -/
abbrev ipcStdioConfig : IO.Process.StdioConfig :=
  {stdin := .piped, stdout := .piped, stderr := .inherit}

abbrev RequestMap (α) := Std.RBMap RequestID α compare

/- State for an `LsWorker`. -/
structure LsState where
  nextID : Nat := 0
  messages : IO.AsyncList IO.Error Message := .nil
  pendingResponses : RequestMap Json := {}

namespace LsState

def saveResponse (id : RequestID) (result : Json) (self : LsState) : LsState :=
  {self with pendingResponses := self.pendingResponses.insert id result}

-- TODO: Handle non-response messages
partial def waitResponse (expectedID : RequestID)
: ExceptT IO.Error (StateM LsState) (Option Json) := do
  match (← get).messages with
  | .nil => return .none
  | .cons m ms =>
    modify ({· with messages := ms})
    if let .response id result := m then
      if id == expectedID then
        return result
      else
        modify (·.saveResponse id result)
    waitResponse expectedID
  | .delayed ms =>
    modify ({· with messages := ← ms.get})
    waitResponse expectedID

def takePendingResponse? (id : RequestID) : StateM LsState <| Option Json := fun self => do
  if let some result := self.pendingResponses.find? id then
    (.some result, {self with pendingResponses := self.pendingResponses.erase id})
  else
    (.none, self)

def getOrWaitResponse (expectedID : RequestID)
: ExceptT IO.Error (StateM LsState) (Option Json) := do
  if let some result ← takePendingResponse? expectedID then
    return result
  waitResponse expectedID

end LsState

/-- A running language server process -/
structure LsWorker where
  child : IO.Process.Child ipcStdioConfig
  state : IO.Ref LsState
  capabilities : ServerCapabilities := {}
  info? : Option ServerInfo := none

namespace LsWorker

def stdin (self : LsWorker) : IO.FS.Stream :=
  IO.FS.Stream.ofHandle self.child.stdin

def stdout (self : LsWorker) : IO.FS.Stream :=
  IO.FS.Stream.ofHandle self.child.stdout

def notify [ToJson α] (self : LsWorker) (method : String) (param : α) : IO PUnit := do
  self.stdin.writeLspNotification {method, param}

def withTextDocument [Monad m] [MonadLiftT IO m] [MonadFinally m]
(self : LsWorker) (uri : DocumentUri) (text : String) (languageId : String)
(act : m α) (version := 0) : m α := do
  let dop : DidOpenTextDocumentParams := ⟨{uri, version, text, languageId}⟩
  self.notify "textDocument/didOpen" dop
  try
    act
  finally
    let dcp : DidCloseTextDocumentParams := ⟨⟨uri⟩⟩
    self.notify "textDocument/didClose" dcp

def nextID (self : LsWorker) : BaseIO RequestID :=
  self.state.modifyGet fun s => (s.nextID, {s with nextID := s.nextID + 1})

def request [ToJson α] (self : LsWorker) (method : String) (param : α) : IO RequestID := do
  let id ← self.nextID
  self.stdin.writeLspRequest {id, method, param}
  return id

def takePendingResponse? (self : LsWorker) (id : RequestID) : BaseIO (Option Json) := do
  self.state.modifyGet fun s =>
    let rs := s.pendingResponses
    match rs.find? id with
    | some result => (some result, {s with pendingResponses := rs.erase id})
    | none => (none, s)

def readUntilResponse (self : LsWorker) (expectedID : RequestID) : IO Json := do
  match (← self.state.modifyGet <| LsState.getOrWaitResponse expectedID) with
  | .ok (some result) => return result
  | .ok none => throw <| IO.userError <|
    s!"Language server quit without responding to request `{expectedID}`"
  | .error e => throw e

def readUntilResponseAs (self : LsWorker) (expectedID : RequestID) (α) [FromJson α] : IO α := do
  let result ← self.readUntilResponse expectedID
  match fromJson? result with
  | .ok v => pure v
  | .error inner => throw <| IO.userError <|
    s!"Unexpected result for request `{expectedID}`:\n'{result.compress}'\n{inner}"

def call [ToJson α] [FromJson β] (self : LsWorker) (method : String) (param : α) : IO β := do
  let id ← self.request method param
  self.readUntilResponseAs id β

/-- Auxiliary function for `readMessagesAsync` -/
partial def readMessagesAsyncAux (self : LsWorker)
: BaseIO (Task <| Except IO.Error <| IO.AsyncList IO.Error Message) := IO.asTask do
  let msg ← self.stdout.readLspMessage
  let nextTask ← self.readMessagesAsyncAux
  return .cons msg <| .delayed nextTask

/-- Asynchronously read all messages from the language server and build a lazy list of them. -/
def readMessagesAsync (self : LsWorker) : BaseIO (IO.AsyncList IO.Error Message) :=
  .delayed <$> self.readMessagesAsyncAux

def init (cmd : String) (args : Array String := #[]) (params : InitializeParams) : IO LsWorker := do
  let child ← IO.Process.spawn {cmd, args, toStdioConfig := ipcStdioConfig}
  let ls : LsWorker := {child, state := ← IO.mkRef {}}
  let messages ← ls.readMessagesAsync
  ls.state.modify ({· with messages})
  let (⟨capabilities, info?⟩ : InitializeResult) ← ls.call "initialize" params
  ls.notify "initialized" InitializedParams.mk
  return {ls with capabilities, info?}

def exit (self : LsWorker) : IO PUnit := do
  let (_ : Json) ← self.call "shutdown" Json.null
  self.notify "exit" Json.null
