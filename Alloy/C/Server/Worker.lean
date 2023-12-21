/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Util.Path
import Lean.Data.LOption
import Alloy.C.Server.Clangd
import Alloy.Util.Server

/-!
# C Language Server Worker

The language server can be in one of three states:
* `none`: Failed to start and is therefore unsupported
* `some`: Initialized and is running
* `undef`: Not yet started, waiting for the first `getLs?` to attempt start

We use the limbo state of `undef` to ensure we do not try to start the
language server when unnecessary (e.g., during compilation, where elaboration
is non-interactive).
-/

open Lean Lsp

namespace Alloy.C

/-- Verbosity of the log messages `clangd` writes. -/
inductive ClangdLogging
| /-- Error messages only -/ error
| /-- High level execution tracking -/ info
| /-- Low level details -/ verbose

instance : Inhabited ClangdLogging := ⟨.info⟩

def ClangdLogging.toString : ClangdLogging → String
| .error => "error"
| .info => "info"
| .verbose => "verbose"

instance : ToString ClangdLogging := ⟨ClangdLogging.toString⟩

/-- Alloy `clangd` language server configuration. -/
structure ServerConfig where
  /--
  Flags to configure the `clangd` server with.
  Alloy adds Lean's include directory to the include path by default.
  -/
  flags : Array String
  /--Log message verbosity of the `clangd` language server. -/
  logging : ClangdLogging
  deriving Inhabited

structure ClangdDocState where
  leanVer : Nat := 0
  shimEndPos : String.Pos := 0
  version : Nat := 0
  ready? : Option (IO.Promise Unit) := none
  diagnostics : Sum (Array Diagnostic) (IO.Promise (Array Diagnostic)) := .inl #[]
  deriving Inhabited

abbrev ClangdWorker := LsWorker ClangdDocState

/-- Attempt to initialize the Alloy `clangd` server. -/
def initLs? (cfg : ServerConfig) : BaseIO (Option ClangdWorker) :=
  let act : IO ClangdWorker := do
    let args := #[s!"--log={cfg.logging}",
      -- We follow Lean's example and do not limit completion results.
      "--limit-results=0", "--header-insertion=never"]
    let ls ← LsWorker.init {} "clangd" args {
      capabilities := {
        textDocument? := some {
          hover? := some {
            contentFormat? := some #[.markdown, .plaintext]
          },
          declaration? := some {
            linkSupport? := true
          }
          completion? := some {
            completionItem? := some {
              documentationFormat? := some #[.markdown, .plaintext]
              insertReplaceSupport? := true
            }
            completionItemKind? := some {
              valueSet? := some #[
                .text, .method, .function, .constructor, .field, .variable,
                .class,  .interface, .module, .property, .unit, .value, .enum,
                .keyword, .snippet,  .color, .file, .reference, .folder, .enumMember,
                .constant, .struct, .event,  .operator, .typeParameter
              ]
            }
          }
        }
      }
      initializationOptions? := some <| toJson (α := Clangd.InitializationOptions) {
        -- Add Lean's include directory to `clangd`'s include path
        fallbackFlags? := some cfg.flags
        clangdFileStatus? := true
      }
    }
    ls.setNotificationHandler "textDocument/clangd.fileStatus" fun ps => do
      unless ps.state = "idle" do
        return
      let p? ← ls.state.atomically fun ref => ref.modifyGet fun s =>
        (s.data.ready?, {s with data.ready? := none})
      if let some p := p? then
        p.resolve ()
    ls.setNotificationHandler "textDocument/publishDiagnostics" fun ps => do
      let p? ← ls.state.atomically fun ref => ref.modifyGet fun s =>
        let p? :=
          match s.data.diagnostics with
          | .inl _ => none
          | .inr p => some p
        (p?, {s with data.diagnostics := .inl ps.diagnostics})
      if let some p := p? then
        p.resolve ps.diagnostics
    return ls
  (some <$> act).catchExceptions fun e => do
    IO.eprintln s!"Failed to initialize Alloy C language server: {e}"
      |>.catchExceptions (fun _ => pure ())
    return none

def ClangdWorker.updateDocumentVersion
  (ls : ClangdWorker) (leanVer : Nat) (text : String)
: IO (Nat × Bool) := do
  ls.state.atomically fun ref => do
    let s ← ref.get
    let doc := s.data
    -- NOTE: Assumes shim changes across snapshots lengthen the shim
    unless leanVer = 0 || doc.leanVer < leanVer || doc.shimEndPos < text.endPos do
      return (doc.version, false)
    let doc := {
      leanVer := if leanVer > 0 then leanVer else doc.leanVer
      shimEndPos := text.endPos
      version := doc.version + 1
      ready? := ← do
        match doc.ready? with
        | none => some <$> IO.Promise.new
        | some p => pure <| some p
      diagnostics := ← do
        match doc.diagnostics with
        | .inl _ => .inr <$> IO.Promise.new
        | .inr p => pure <| .inr p
    }
    ref.set {s with data := doc}
    return (doc.version, true)

def ClangdWorker.setShimDocument
  (ls : ClangdWorker) (leanVer : Nat) (text : String) (languageId : String)
: IO Unit := do
  let (version, updated) ← ls.updateDocumentVersion leanVer text
  if updated then
    ls.notify "textDocument/didOpen" ⟨{uri := nullUri, version, text, languageId}⟩

def ClangdWorker.readyShimDocument
  (ls : ClangdWorker) (timeout : UInt32 := 0)
: IO Unit := do
  if let some p ← ls.state.atomically (·.get <&> (·.data.ready?)) then
    if timeout > 0 then
      unless  (← wait? p.result timeout).isSome do
        throw <| .userError <|
          "clangd took to long to ready the shim document"
    else
      IO.wait p.result

def ClangdWorker.collectShimDiagnostics
  (ls : ClangdWorker) (timeout : UInt32 := 0)
: IO (Array Diagnostic) := do
  match (← ls.state.atomically (·.get <&> (·.data.diagnostics))) with
  | .inl ds => return ds
  | .inr p =>
    if timeout > 0 then
      if let some diagnostics ← wait? p.result timeout then
        return diagnostics
      else
        throw <| .userError <|
          "Could not collect clangd diagnostics for the shim: " ++
          "clangd took too long to load the document"
    else
      IO.wait p.result

/-- Configuration with which to start Alloy's C language server. -/
initialize serverConfig : IO.Ref ServerConfig ← do IO.mkRef {
  flags := #["-I", (← Lean.getBuildDir) / "include" |>.toString]
  logging := .error
}

initialize serverMux : IO.Mutex (LOption ClangdWorker) ← IO.Mutex.new .undef

/-- Return the C language server. Tries to start it exactly once. -/
def getLs? : BaseIO (Option ClangdWorker) :=
  serverMux.atomically fun ref => ref.get >>= fun
    | .none => return none
    | .some ls => return some ls
    | .undef => do
      let ls? ← initLs? (← serverConfig.get)
      ref.set ls?.toLOption
      return ls?

/-- Return the C language server if it has already started. -/
def getStartedLs? : BaseIO (Option ClangdWorker) :=
  serverMux.atomically fun ref => ref.get >>= fun
    | .some ls => return some ls
    | _ => return none
