/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Server.AsyncList
import Lean.Data.Lsp.Communication
import Alloy.Util.Server.Initialize

open Lean hiding Message
open Lean.Lsp Lean.JsonRpc
open IO (Promise)

namespace Alloy

/-- Like `Lean.Lsp.Ipc.ipcStdioConfig`, but an `abbrev`. -/
abbrev ipcStdioConfig : IO.Process.StdioConfig :=
  {stdin := .piped, stdout := .piped, stderr := .inherit}

/-- An LSP response error. -/
structure LsError where
  code : ErrorCode
  message : String
  data? : Option Json

/-- State for an `LsWorker`. -/
structure LsState where
  nextID : Nat := 0
  responseMap : RBMap RequestID (Promise (Except LsError Json)) compare := {}
  error? : Option IO.Error := none

/-- A running language server process -/
structure LsWorker where
  child : IO.Process.Child ipcStdioConfig
  state : IO.Mutex LsState
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

def call [ToJson α] [FromJson β] (self : LsWorker) (method : String) (param : α) : IO β := do
  let (id, p) ← self.state.atomically fun ref => do
    let s ← ref.get
    if let some e := s.error? then
      throw <| IO.userError <| s!"Language server error: {e}"
    let id := s.nextID
    self.stdin.writeLspRequest {id, method, param}
    let p ← Promise.new
    ref.set {s with nextID := id + 1, responseMap := s.responseMap.insert id p}
    return (id, p)
  let r ← IO.wait p.result
  self.state.atomically (·.modify fun s => {s with responseMap := s.responseMap.erase id})
  match r with
  | .ok r =>
    match fromJson? r with
    | .ok v => pure v
    | .error inner => throw <| IO.userError <|
      s!"Unexpected result for request `{id}`:\n'{r.compress}'\n{inner}"
  | .error e => throw <| IO.userError e.message

/--
Read all LSP messages from `stream`, completing requests from `state`.
TODO: Handle non-response messages.
-/
partial def readLspMessages (stream : IO.FS.Stream) (state : IO.Mutex LsState) : BaseIO Unit := do
  match (← stream.readLspMessage.toBaseIO) with
  | .ok msg =>
    match msg with
    | .response id result =>
      if let some p ← state.atomically (·.get <&> (·.responseMap.find? id)) then
        p.resolve <| .ok result
    | .responseError id code message data? =>
      if let some p ← state.atomically (·.get <&> (·.responseMap.find? id)) then
        p.resolve <| .error {code, message, data?}
    | _ => pure ()
    readLspMessages stream state
  | .error e =>
    state.atomically fun ref => do
      let s ← ref.get
      for (_ , p) in s.responseMap do
        p.resolve <| .error <| LsError.mk ErrorCode.internalError
          s!"Language server terminated without responding to request" none
      ref.set {s with error? := e}

/-- Spawn the worker process and initialize the language server. -/
def init (cmd : String) (args : Array String := #[]) (params : InitializeParams) : IO LsWorker := do
  let child ← IO.Process.spawn {cmd, args, toStdioConfig := ipcStdioConfig}
  let state : IO.Mutex LsState ← IO.Mutex.new {}
  discard <| BaseIO.asTask <|
    readLspMessages (IO.FS.Stream.ofHandle child.stdout) state
  let ls : LsWorker := {child, state}
  let (⟨capabilities, info?⟩ : InitializeResult) ← ls.call "initialize" params
  ls.notify "initialized" InitializedParams.mk
  return {ls with capabilities, info?}

/-- Exit the language server. -/
def exit (self : LsWorker) : IO PUnit := do
  let (_ : Json) ← self.call "shutdown" Json.null
  self.notify "exit" Json.null
