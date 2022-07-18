/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Extension
import Alloy.Util.Server

open System Lean Server Lsp RequestM JsonRpc

namespace Alloy.C

--------------------------------------------------------------------------------
/-! ## `clangd` Initialization Options                                        -/
--------------------------------------------------------------------------------

structure CompilationDatabaseEntry where
  workingDirectory : String
  compilationCommand : Array String
  deriving ToJson, FromJson

abbrev CompilationDatabase :=
  Std.RBMap FilePath CompilationDatabaseEntry
    (compare ·.toString ·.toString)

instance : ToJson CompilationDatabase where
  toJson v := Json.obj <| v.fold (init := .leaf) fun a k v =>
    a.insert compare k.toString (toJson v)

instance : FromJson CompilationDatabase where
  fromJson? v := do
    let o ← v.getObj?
    o.foldM (init := {}) fun a k v => do
      return a.insert (FilePath.mk k) (← fromJson? v)

structure ClangdInitializationOptions where
  fallbackFlags? : Option (Array String) := none
  compilationDatabasePath? : Option String := none
  compilationDatabaseChanges? : Option CompilationDatabase := none
  deriving ToJson, FromJson

--------------------------------------------------------------------------------
/-! ## C Language Server Worker                                               -/
--------------------------------------------------------------------------------

/-
The language server can be in one of three states:
* `none`: Failed to start and is thus unsupported
* `some`: Initialized and is running
* `undef`: Not yet started, waiting for the first `getLs?` to attempt start

We use the limbo state of `undef` to ensure we do not try to start the
language server when not needing (e.g., during non-interactive elaboration
like compilation).
-/
initialize serverRef : IO.Ref (LOption LsWorker) ← IO.mkRef .undef

def initLs? : IO (Option LsWorker) :=
  try some <$> do
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
        fallbackFlags? := some #["-I", (← getBuildDir) / "include" |>.toString]
      }
    }
  catch e =>
    IO.eprintln s!"Failed to initialize Alloy C language server: {e}"
    return none

def getLs? : IO (Option LsWorker) := do
  match (← serverRef.get) with
  | .none => pure none
  | .some ls => pure <| some ls
  | .undef =>
    let ls? ← initLs?
    serverRef.modifyGet fun
      /- We match again because another task could have out-raced us. -/
      | .none => (.none, .none)
      | .some ls => (.some ls, .some ls)
      | .undef => (ls?, ls?.toLOption)

--------------------------------------------------------------------------------
/-! ## Handling Requests                                                      -/
--------------------------------------------------------------------------------

def Shim.findLspPos? (self : Shim) (leanPos : String.Pos) : Option Lsp.Position := do
  self.text.utf8PosToLspPos (← self.findPos? leanPos)

def handleHover
(p : HoverParams) (prev : RequestTask (Option Hover))
: RequestM (RequestTask (Option Hover)) := do
  let doc ← readDoc
  let text := doc.meta.text
  let hoverPos := text.lspPosToUtf8Pos p.position
  bindWaitFindSnap doc (·.endPos > hoverPos) (notFoundX := pure prev) fun snap => do
    let shim := getLocalShim snap.env
    let some hoverCPos := shim.findLspPos? hoverPos | return prev
    let some ls ← getLs? | return prev
    let chp : HoverParams := {
      textDocument := ⟨nullUri⟩,
      position := hoverCPos
    }
    ls.withTextDocument nullUri shim.toString "c" do
      try
        let id ← ls.request "textDocument/hover" chp
        let hover? ← ls.readUntilResponseAs id (Option Hover)
        unless hover?.isSome do return prev
        return Task.pure <| .ok <| hover?
      catch e =>
        (←read).hLog.putStrLn s!"C language server request failed: {e.message}"
        return prev

initialize
  forceChainLspRequestHandler "textDocument/hover" HoverParams (Option Hover) handleHover
