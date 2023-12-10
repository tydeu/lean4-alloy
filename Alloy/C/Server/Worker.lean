/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Util.Path
import Lean.Data.LOption
import Alloy.C.Server.Clangd
import Alloy.Util.Server.Worker

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

open Lean

namespace Alloy.C

/-- Alloy `clangd` language server configuration. -/
structure ServerConfig where
  /--
  C flags to configure the `clangd` server with.
  Alloy adds Lean's include directory to the include path by default.
  -/
  flags : Array String
  /-- Log level of the `clangd` language server. -/
  logging : String
  deriving Inhabited

/-- Attempt to initialize the Alloy `clangd` server. -/
def initLs? (cfg : ServerConfig) : BaseIO (Option LsWorker) :=
  let act := some <$> do
    let args := #[s!"--log={cfg.logging}",
      -- We follow Lean's example and do not limit completion results.
      "--limit-results=0", "--header-insertion=never"]
    let errorFilter (line : String) :=
      -- HACK: Filter "Trying to remove file from TUScheduler that is not tracked: <file>" errors
      -- TODO: Actually fix the underlying cause -- `withTextDocument` race conditions
      line.dropRightWhile (· ≠ ':') |>.endsWith "not tracked:"
    LsWorker.init "clangd" args (errorFilter := errorFilter) {
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
  act.catchExceptions fun e => do
    IO.eprintln s!"Failed to initialize Alloy C language server: {e}"
      |>.catchExceptions (fun _ => pure ())
    return none

/-- Configuration with which to start Alloy's C language server. -/
initialize serverConfig : IO.Ref ServerConfig ← do IO.mkRef {
  flags := #["-I", (← Lean.getBuildDir) / "include" |>.toString]
  logging := "error"
}

initialize serverMux : IO.Mutex (LOption LsWorker) ← IO.Mutex.new .undef

/-- Return the C language server. Tries to start it exactly once. -/
def getLs? : BaseIO (Option LsWorker) :=
  serverMux.atomically fun ref => ref.get >>= fun
    | .none => return none
    | .some ls => return some ls
    | .undef => do
      let ls? ← initLs? (← serverConfig.get)
      ref.set ls?.toLOption
      return ls?
