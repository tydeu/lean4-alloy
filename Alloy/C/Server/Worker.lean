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
* `none`: Failed to start and is thus unsupported
* `some`: Initialized and is running
* `undef`: Not yet started, waiting for the first `getLs?` to attempt start

We use the limbo state of `undef` to ensure we do not try to start the
language server when not needing (e.g., during non-interactive elaboration
like compilation).
-/

open Lean

namespace Alloy.C

initialize serverRef : IO.Ref (LOption LsWorker) ← IO.mkRef .undef

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

def getLs? : BaseIO (Option LsWorker) := do
  let ls? : LOption _ ← serverRef.modifyGet fun
  | .none => (.none, .none)
  | .some ls => (.some ls, .some ls)
  | .undef => (.undef, .none) -- locks the reference
  match ls? with
  | .none => return none
  | .some ls => return some ls
  | .undef =>
    let some ls ← initLs? | return none
    serverRef.set (.some ls)
    return some ls
