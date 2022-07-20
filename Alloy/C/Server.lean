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
        fallbackFlags? := some #["-I", (← getBuildDir) / "include" |>.toString]
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

--------------------------------------------------------------------------------
/-! ## Handling Requests                                                      -/
--------------------------------------------------------------------------------

def Shim.leanPosToCLsp? (self : Shim) (leanPos : String.Pos) : Option Lsp.Position := do
  self.text.utf8PosToLspPos (← self.leanPosToC? leanPos)

def Shim.cLspPosToLean? (self : Shim) (cPos : Lsp.Position) : Option String.Pos := do
  self.cPosToLean? (self.text.lspPosToUtf8Pos cPos)

def Shim.cPosToLeanLsp? (self : Shim) (cPos : String.Pos) (leanText : FileMap) : Option Lsp.Position := do
  leanText.utf8PosToLspPos (← self.cPosToLean? cPos)

def Shim.cLspPosToLeanLsp? (self : Shim) (cPos : Lsp.Position) (leanText : FileMap) : Option Lsp.Position := do
  leanText.utf8PosToLspPos (← self.cLspPosToLean? cPos)

def Shim.cLspRangeToLeanLsp? (self : Shim) (cRange : Lsp.Range) (leanText : FileMap) : Option Lsp.Range := do
  let startPos ← self.cLspPosToLeanLsp? cRange.start leanText
  let beforeEndPos := self.text.source.prev (self.text.lspPosToUtf8Pos cRange.end)
  let beforeEndPos := self.text.source.next (← self.cPosToLean? beforeEndPos)
  return ⟨startPos, leanText.utf8PosToLspPos beforeEndPos⟩

/-- Fallback to returning `resp` if `act` errors. Also, log the error message. -/
def withFallbackResponse (resp : RequestTask α) (act : RequestM (RequestTask α)) : RequestM (RequestTask α) :=
  try
    act
  catch e =>
    (←read).hLog.putStrLn s!"C language server request failed: {e.message}"
    return resp

def handleHover
(p : HoverParams) (prev : RequestTask (Option Hover))
: RequestM (RequestTask (Option Hover)) := do
  let doc ← readDoc
  let text := doc.meta.text
  let hoverPos := text.lspPosToUtf8Pos p.position
  bindWaitFindSnap doc (·.endPos > hoverPos) (notFoundX := pure prev) fun snap => do
    let shim := getLocalShim snap.env
    let some hoverCPos := shim.leanPosToCLsp? hoverPos | return prev
    let some ls ← getLs? | return prev
    withFallbackResponse prev do
      ls.withTextDocument nullUri shim.toString "c" do
        let hover? ← ls.call "textDocument/hover" (α := HoverParams) {
          textDocument := ⟨nullUri⟩,
          position := hoverCPos
        }
        let some (cHover : Hover) := hover? | return prev
        bindTask prev fun x => do
          if let some leanHover := x.toOption.bind (·) then
            let range? := leanHover.range? <|> cHover.range?
            let value := s!"{leanHover.contents.value}\n\n---\n\n{cHover.contents.value}"
            return Task.pure <| .ok <| some ⟨{kind := .markdown, value}, range?⟩
          else
            if let some range := cHover.range? then
              let some range := shim.cLspRangeToLeanLsp? range text
                | return Task.pure x
              return Task.pure <| .ok <| some {cHover with range? := some range}
            else
              return Task.pure <| .ok <| some cHover

def bitMaskToArray (legend : Array α) (mask : Nat) : Array α := Id.run do
  let mut mods := #[]
  for h : i in [0:legend.size] do
    have : i < legend.size := h.upper
    if (mask >>> i) % 2 = 1 then
      mods := mods.push <| legend[i]
  return mods

def bitMaskOfArray [BEq α] (legend : Array α) (as : Array α) : Nat := Id.run do
  let mut mask := 0
  for h : i in [0:legend.size] do
    have : i < legend.size := h.upper
    if as.contains legend[i] then
      mask := mask ||| (1 <<< i)
  return mask

structure SemanticTokenEntry where
  line : Nat
  startChar : Nat
  length : Nat
  type : Nat
  modifierMask : Nat
  deriving Inhabited, Repr

protected def SemanticTokenEntry.ordLt (a b : SemanticTokenEntry) : Bool:=
  if a.line = b.line then a.startChar < b.startChar else a.line < b.line

def processLeanTokens (data : Array Nat) : Array SemanticTokenEntry := Id.run do
  let mut line := 0
  let mut char := 0
  let mut entries : Array SemanticTokenEntry := #[]
  for i in [0:data.size:5] do
    let #[deltaLine, deltaStart, len, type, modMask] := data[i:i+5].toArray
      | return entries -- If this happens, something is wrong with Lean, but we don't really care
    line := line + deltaLine
    char := if deltaLine = 0 then char + deltaStart else deltaStart
    entries := entries.push ⟨line, char, len, type, modMask⟩
  return entries

def processCTokens (data : Array Nat) (shim : Shim) (text : FileMap)
(beginPos endPos : String.Pos) (types modifiers : Array String)
: Array SemanticTokenEntry := Id.run do
  let mut line := 0
  let mut char := 0
  let mut entries : Array SemanticTokenEntry := #[]
  for i in [0:data.size:5] do
    let #[deltaLine, deltaStart, len, type, modMask] := data[i:i+5].toArray
      | return entries -- We are being fault tolerant here, maybe we shouldn't be
    line := line + deltaLine
    char := if deltaLine = 0 then char + deltaStart else deltaStart
    let cPos := shim.text.lspPosToUtf8Pos ⟨line, char⟩
    let some pos := shim.cPosToLean? cPos
      | continue -- Ditto
    unless beginPos < pos && pos < endPos do
      continue
    let leanLen :=
      shim.cPosToLean? (cPos + ⟨len - 1⟩) |>.map
      (fun p => (p - pos).byteIdx + 1) |>.getD len
    let lspPos := text.utf8PosToLspPos pos
    let mods := bitMaskToArray modifiers modMask
    let modMask := bitMaskOfArray SemanticTokenModifier.names mods
    let some type := types[type]?
      | continue -- Ditto
    let some type := SemanticTokenType.names.getIdx? type
      | continue -- Skip semantic token types not supported by the Lean server
    entries := entries.push ⟨lspPos.line, lspPos.character, leanLen, type, modMask⟩
  return entries

def processTokenEntries (entries : Array SemanticTokenEntry) : Array Nat := Id.run do
  let mut data := #[]
  let mut lastLine := 0
  let mut lastChar := 0
  for ⟨line, char, len, type, modMask⟩ in entries do
    let deltaLine := line - lastLine
    let deltaStart := if line = lastLine then char - lastChar else char
    data := data ++ #[deltaLine, deltaStart, len, type, modMask]
    lastLine := line; lastChar := char
  return data

def handleSemanticTokens
(beginPos endPos : String.Pos) (prev : RequestTask SemanticTokens)
: RequestM (RequestTask SemanticTokens) := do
  let doc ← readDoc
  let afterEnd snap := snap.isAtEnd || snap.beginPos > endPos
  bindWaitFindSnap doc afterEnd (notFoundX := pure prev) fun snap => do
    let shim := getLocalShim snap.env
    if shim.isEmpty then do return prev
    let some ls ← getLs? | return prev
    let some provider := ls.capabilities.semanticTokensProvider? | return prev
    let {tokenTypes, tokenModifiers} := provider.legend
    withFallbackResponse prev do
      ls.withTextDocument nullUri shim.toString "c" do
        let tokens ← ls.call "textDocument/semanticTokens/full"
          (α := SemanticTokensParams) (β := SemanticTokens) ⟨⟨nullUri⟩⟩
        let cEntries := processCTokens tokens.data
          shim doc.meta.text beginPos endPos tokenTypes tokenModifiers
        bindTask prev fun
        | .error e => throw e
        | .ok tokens => do
          let leanEntries := processLeanTokens tokens.data
          let sortedEntries := cEntries ++ leanEntries |>.qsort SemanticTokenEntry.ordLt
          let data := processTokenEntries sortedEntries
          return Task.pure <| .ok {tokens with data}

def handleSemanticTokensFull
(_ : SemanticTokensParams) (prev : RequestTask SemanticTokens)
: RequestM (RequestTask SemanticTokens) := do
  handleSemanticTokens 0 ⟨1 <<< 16⟩ prev

def handleSemanticTokensRange
(p : SemanticTokensRangeParams) (prev : RequestTask SemanticTokens)
: RequestM (RequestTask SemanticTokens) := do
  let doc ← readDoc
  let text := doc.meta.text
  let beginPos := text.lspPosToUtf8Pos p.range.start
  let endPos := text.lspPosToUtf8Pos p.range.end
  handleSemanticTokens beginPos endPos prev

initialize
  chainLspRequestHandler "textDocument/hover" HoverParams (Option Hover) handleHover
  chainLspRequestHandler "textDocument/semanticTokens/full"  SemanticTokensParams SemanticTokens handleSemanticTokensFull
  chainLspRequestHandler "textDocument/semanticTokens/range" SemanticTokensRangeParams SemanticTokens handleSemanticTokensRange
