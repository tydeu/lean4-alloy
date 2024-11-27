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
# Alloy's C Language Server Worker

Alloy uses `clangd` to provide interactive support for C shims.
This file contains the `ClangdWorker` specialization of Alloy's general
`LsWorker` type with additional configuration and helpers for managing
precisely an Alloy-style `clangd` language server.

It also contains the definitions for the the `ClangdWorker` instance
used by Alloy in the Lean language server. The instance is also reused during
command elaboration to provide diagnostics for C shim code.
-/

open System Lean Lsp

namespace Alloy.C

/-! ## Clangd-specific Worker Configuration & Helpers -/

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

/-- `clangd` language server configuration. -/
structure ServerConfig where
  /--
  Array of directories to add to `clangd`'s include path
  (e.g., directories to search within for `#include`'d files).

  Alloy, by default, already also adds Lean's own include directory.
  Additional include paths can be added via the `addServerIncludePath` helper.
  -/
  moreIncludePaths : Array FilePath
  /--
  Additional Flags to configure the `clangd` server with
  (on top of those implied by  `includePaths`).
  -/
  moreFlags : Array String
  /--Log message verbosity of the `clangd` language server. -/
  logging : ClangdLogging
  deriving Inhabited

@[inline] def ServerConfig.flags (cfg : ServerConfig) : Array String :=
  cfg.moreIncludePaths.map (s!"-I{·}") ++ cfg.moreFlags

@[inline] def ServerConfig.addFlags (cfg : ServerConfig) (flags : Array String) : ServerConfig :=
  {cfg with moreFlags := cfg.moreFlags ++ flags}

@[inline] def ServerConfig.addFlag (cfg : ServerConfig) (flag : String) : ServerConfig :=
  {cfg with moreFlags := cfg.moreFlags.push flag}

@[inline] def ServerConfig.addIncludePath (cfg : ServerConfig) (path : FilePath) : ServerConfig :=
  {cfg with moreIncludePaths := cfg.moreIncludePaths.push path}

@[inline] def ServerConfig.setLogging (cfg : ServerConfig) (logging : ClangdLogging) : ServerConfig :=
  {cfg with logging}

attribute [instance] Sum.inhabitedLeft in
structure ClangdDocState where
  leanVer : Nat := 0
  shimEndPos : String.Pos := 0
  version : Nat := 0
  ready? : Option (IO.Promise Unit) := none
  diagnostics : Sum (Array Diagnostic) (IO.Promise (Array Diagnostic)) := .inl #[]
  deriving Inhabited

abbrev ClangdWorker := LsWorker ClangdDocState

/-- Attempt to initialize an Alloy `clangd` server. -/
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
        fallbackFlags? := some <| #["-I", (← Lean.getBuildDir) / "include" |>.toString] ++ cfg.flags
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
: IO (Option Nat) := do
  ls.state.atomically fun ref => do
    let s ← ref.get
    let doc := s.data
    -- NOTE: Assumes shim changes across snapshots lengthen the shim
    unless leanVer = 0 || doc.leanVer < leanVer || doc.shimEndPos < text.endPos do
      return none
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
    return some doc.version

def ClangdWorker.setShimDocument
  (ls : ClangdWorker) (leanVer : Nat) (text : String) (languageId : String)
: IO Unit := do
  if let some version ← ls.updateDocumentVersion leanVer text then
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

/-! ## Language Server Instance

The language server used by Alloy can be in one of three states:
* `none`: Failed to start and is therefore unsupported
* `some`: Initialized and is running
* `undef`: Not yet started, waiting for the first `getLs?` to attempt start

We use the limbo state of `undef` to ensure we do not try to start the
language server when unnecessary (e.g., during compilation, where elaboration
is non-interactive).
-/

/--
The environment extension which stores a reference to
the current instance of Alloy's C language sever.

It is wrapped in a mutex because it is has complex volatile state,
but accessed concurrently by the Lean language server.

It is stored in an environment extension so that a command can restart
the server for future commands by swapping out the reference while still
preserving the previous instance for past commands.
-/
initialize serverExt : EnvExtension (Option (IO.Mutex (LOption ClangdWorker))) ←
  registerEnvExtension (some <$> IO.Mutex.new .undef)

/-- Configuration with which to start Alloy's C language server instance. -/
initialize serverConfig : IO.Ref ServerConfig ← do IO.mkRef {
  moreIncludePaths := #[]
  moreFlags := #[]
  logging := .error
}

/--
Return Alloy's C language server.
Tries to start it exactly once across all calls to this function.
-/
def getLs? (env : Environment) : BaseIO (Option ClangdWorker) := do
  let some mux := serverExt.getState env | return none
  mux.atomically fun ref => ref.get >>= fun
    | .none => return none
    | .some ls => return some ls
    | .undef => do
      let ls? ← initLs? (← serverConfig.get)
      ref.set ls?.toLOption
      return ls?

/-- Return Alloy's C language server if it has already started. -/
def getStartedLs? (env : Environment) : BaseIO (Option ClangdWorker) := do
  let some mux := serverExt.getState env | return none
  mux.atomically fun ref => ref.get >>= fun
    | .some ls => return some ls
    | _ => return none

/-!
### Server Configuration

When writing C code, you may wish to further configure the language server to
provide better interactive features. For example, you may wish to add additional
locations to its include path so that go-to-def and the like work function on
included symbols. To accomplish, there are two ways to modify the configuration
used by Alloy's C  language server.

#### Approach 1: Global Change

One approach is to use an `initialize` command to modify the configuration
during module initialization. This will apply the change to any module that
imports the module with the initializer. For example:

**Foo.lean**
```
import Alloy.C
initialize Alloy.C.modifyGlobalServerConfig (·.addIncludePath "/lib/foo/include")
```

**Bar.lean**
```
import Foo
alloy c include "foo.h" -- will also search in `/lib/foo/include`
```

#### Approach 2: Local Change

Alternatively, one can modify the configuration used in a single module via
an `#eval` command:

```
import Alloy.C
#eval Alloy.C.modifyLocalServerConfig (·.addIncludePath "/lib/foo/include")
alloy c include "foo.h" -- will also search in `/lib/foo/include`
```
-/

/--
Modifies the default configuration of Alloy's C language server.
Suited for use in an `initialize` block.
See the note above on "Server COnfiguration" for more details.
-/
@[inline] def modifyGlobalServerConfig (f : ServerConfig → ServerConfig) : BaseIO PUnit := do
  serverConfig.modify f

/--
Restarts Alloy's C language server with a modified configuration.
Suited for use in an `#eval` command.
See the note above on "Server COnfiguration" for more details.
-/
@[inline] def modifyLocalServerConfig (f : ServerConfig → ServerConfig) : CoreM PUnit := do
  let cfg ← liftM (m := BaseIO) <| serverConfig.get
  let ls? ← initLs? (f cfg)
  let mux ← IO.Mutex.new ls?.toLOption
  modifyEnv fun env => serverExt.setState env mux
