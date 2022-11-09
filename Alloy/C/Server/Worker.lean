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
    LsWorker.init "clangd" #[] <| {
      capabilities := {
        textDocument? := some {
          hover? := some {
            contentFormat? := some #[.markdown, .plaintext]
          }
        }
      }
      initializationOptions? := some <| toJson (α := ClangdInitializationOptions) {
        -- Add Lean's include directory to `clangd`'s include path
        fallbackFlags? := some #["-I", (← Lean.getBuildDir) / "include" |>.toString]
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
