/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
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
  pendingResponses : RequestMap Json := {}

/-- A running language server process -/
structure LsWorker where
  child : IO.Process.Child ipcStdioConfig
  state : IO.Ref LsState

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
  self.notify "textDocument/didOpen"  dop
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

-- TODO: Use separate task to read messages and handle non-response messages
partial def readUntilResponse (self : LsWorker) (expectedID : RequestID) : IO Json := do
  if let some result ← self.takePendingResponse? expectedID then
    return result
  let msg ← self.stdout.readLspMessage
  if let .response id result := msg then
    if id == expectedID then
      return result
    else
      self.state.modify fun s => {s with pendingResponses := s.pendingResponses.insert id result}
  self.readUntilResponse expectedID

def readUntilResponseAs (self : LsWorker) (expectedID : RequestID) (α) [FromJson α] : IO α := do
  let result ← self.readUntilResponse expectedID
  match fromJson? result with
  | .ok v => pure v
  | .error inner => throw <| IO.userError <|
    s!"Unexpected result '{result.compress}'\n{inner}"

def init (cmd : String) (args : Array String := #[]) (params : InitializeParams) : IO LsWorker := do
  let child ← IO.Process.spawn {cmd, args, toStdioConfig := ipcStdioConfig}
  let ls : LsWorker := {child, state := ← IO.mkRef {}}
  let id ← ls.request "initialize" params
  let ⟨_, _⟩ ← ls.stdout.readLspResponseAs id Json
  ls.notify "initialized" InitializedParams.mk
  return ls
