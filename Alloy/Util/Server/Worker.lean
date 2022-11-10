/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Data.Lsp.Communication
import Alloy.Util.Server.Initialize

open Lean hiding Message
open Lean.Lsp Lean.JsonRpc
open IO (Promise)

namespace Alloy

instance [ToString α] : ToString (ResponseError α) where
  toString
    | {id, code, message, data?} =>
      let data := data?.map (s!"\n{·}") |>.getD ""
      s!"Request {id} failed: {message} (code: {toJson code}){data}"

/-- Like `Lean.Lsp.Ipc.ipcStdioConfig`, but an `abbrev`. -/
abbrev ipcStdioConfig : IO.Process.StdioConfig :=
  {stdin := .piped, stdout := .piped, stderr := .inherit}

/-- State for an `LsWorker`. -/
structure LsState where
  nextID : Nat := 0
  responseMap : RBMap RequestID (Promise (Except (ResponseError Json) Json)) compare := {}
  error? : Option IO.Error := none

/-- A running language server process. -/
structure LsWorker where
  child : IO.Process.Child ipcStdioConfig
  state : IO.Mutex LsState
  capabilities : ServerCapabilities := {}
  info? : Option ServerInfo := none

/-- An LSP notification method. -/
class LsNote (method : String) (α : outParam $ Type u)
  extends ToJson α

@[default_instance low] instance : LsNote m Json := {}
instance : LsNote "initialized" InitializedParams := {}
instance : LsNote "textDocument/didOpen" DidOpenTextDocumentParams := {}
instance : LsNote "textDocument/didClose" DidCloseTextDocumentParams := {}

/-- An LSP request/response method. -/
class LsCall (method : String) (α : outParam $ Type u) (β : outParam $ Type w)
  extends ToJson α, FromJson β

@[default_instance low] instance : LsCall m Json Json := {}
instance : LsCall "initialize" InitializeParams InitializeResult := {}
instance : LsCall "textDocument/hover" HoverParams (Option Hover) := {}
instance : LsCall "textDocument/semanticTokens/full"  SemanticTokensParams SemanticTokens := {}
instance : LsCall "textDocument/semanticTokens/range" SemanticTokensRangeParams SemanticTokens := {}

namespace LsWorker

/-- The language server's standard input stream. -/
def stdin (self : LsWorker) : IO.FS.Stream :=
  IO.FS.Stream.ofHandle self.child.stdin

/-- The language server's standard output stream. -/
def stdout (self : LsWorker) : IO.FS.Stream :=
  IO.FS.Stream.ofHandle self.child.stdout

/-- Issue the LSP notification `method` with `param`. -/
def notify (self : LsWorker) (method : String) [LsNote method α] (param : α) : IO Unit := do
  self.stdin.writeLspNotification {method, param}

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
    match r with
    | .ok r =>
      match fromJson? r with
      | .ok v => return .ok v
      | .error e =>
        return .error {
          id, code := .internalError, data? := r
          message := s!"Ill-typed response for request: {e}"
        }
    | .error e => return .error e

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
        p.resolve <| .error {id, code, message, data?}
    | _ => pure ()
    readLspMessages stream state
  | .error e =>
    state.atomically fun ref => do
      let s ← ref.get
      for (id, p) in s.responseMap do
        p.resolve <| .error <| ResponseError.mk id .internalError
          s!"Language server terminated without responding to request" none
      ref.set {s with error? := e}

/-- Spawn the worker process and initialize the language server. -/
def init (cmd : String) (args : Array String := #[]) (params : InitializeParams) : IO LsWorker := do
  let child ← IO.Process.spawn {cmd, args, toStdioConfig := ipcStdioConfig}
  let state : IO.Mutex LsState ← IO.Mutex.new {}
  discard <| BaseIO.asTask <|
    readLspMessages (IO.FS.Stream.ofHandle child.stdout) state
  let ls : LsWorker := {child, state}
  let (⟨capabilities, info?⟩) ← IO.ofExcept
    <| ← IO.wait <| ← ls.call "initialize" params
  ls.notify "initialized" InitializedParams.mk
  return {ls with capabilities, info?}

/-- Exit the language server. -/
def exit (self : LsWorker) : IO Unit := do
  discard <| IO.wait <| ← self.call "shutdown" Json.null
  self.notify "exit" Json.null
