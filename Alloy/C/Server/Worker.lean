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

initialize serverMux : IO.Mutex (LOption LsWorker) ← IO.Mutex.new .undef

def initLs? : BaseIO (Option LsWorker) :=
  let act := some <$> do
    /- NOTE: We follow Lean's example and do not limit completion results. -/
    let args := #["--log=error", "--limit-results=0", "--header-insertion=never"]
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
        fallbackFlags? := some #["-I", (← Lean.getBuildDir) / "include" |>.toString]
        clangdFileStatus? := true
      }
    }
  act.catchExceptions fun e => do
    IO.eprintln s!"Failed to initialize Alloy C language server: {e}"
      |>.catchExceptions (fun _ => pure ())
    return none

def getLs? : BaseIO (Option LsWorker) :=
  serverMux.atomically fun ref => ref.get >>= fun
    | .none => return none
    | .some ls => return some ls
    | .undef => do
      let ls? ← initLs?
      ref.set ls?.toLOption
      return ls?
