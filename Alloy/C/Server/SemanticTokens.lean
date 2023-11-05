/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Shim
import Alloy.C.Server.Worker
import Alloy.Util.Server

open Lean Server Lsp RequestM

namespace Alloy.C

/-! ## Semantic Token Support -/

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

protected def SemanticTokenEntry.ordLt (a b : SemanticTokenEntry) : Bool :=
  a.line < b.line ∨ (a.line = b.line ∧ a.startChar < b.startChar)

def encodeTokenEntries (entries : Array SemanticTokenEntry) : Array Nat := Id.run do
  let mut data := #[]
  let mut lastLine := 0
  let mut lastChar := 0
  for ⟨line, char, len, type, modMask⟩ in entries do
    let deltaLine := line - lastLine
    let deltaStart := if line = lastLine then char - lastChar else char
    data := data ++ #[deltaLine, deltaStart, len, type, modMask]
    lastLine := line; lastChar := char
  return data

def decodeLeanTokens (data : Array Nat) : Array SemanticTokenEntry := Id.run do
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

def decodeShimTokens
(data : Array Nat) (shim : Shim) (text : FileMap)
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
    let shimPos := shim.text.lspPosToUtf8Pos ⟨line, char⟩
    let some pos := shim.shimPosToLeanStx? shimPos >>= (·.getPos?)
      | continue -- Ditto
    let shimTailPos :=
      shim.text.source.codepointPosToUtf8PosFrom shimPos <|
      shim.text.source.utf16PosToCodepointPosFrom len shimPos
    let some tailPos := shim.shimPosToLeanStx? shimTailPos (includeStop := true) >>= (·.getTailPos?)
      | continue
    unless pos < tailPos && beginPos ≤ pos && tailPos ≤ endPos do
      continue
    let lspPos := text.utf8PosToLspPos pos
    let mods := bitMaskToArray modifiers modMask
    let modMask := bitMaskOfArray SemanticTokenModifier.names mods
    let some type := types[type]?
      | continue -- Ditto
    let some type := SemanticTokenType.names.getIdx? type
      | continue -- Skip semantic token types not supported by the Lean server
    let leanLen := text.source.extract pos tailPos |>.length
    let leanLen := shim.text.source.codepointPosToUtf16PosFrom leanLen pos
    entries := entries.push ⟨lspPos.line, lspPos.character, leanLen, type, modMask⟩
  return entries

def handleSemanticTokens
(beginPos endPos : String.Pos) (prev : RequestTask SemanticTokens)
: RequestM (RequestTask SemanticTokens) := do
  let doc ← readDoc
  let afterEnd snap := snap.isAtEnd || snap.beginPos > endPos
  bindWaitFindSnap doc afterEnd (notFoundX := pure prev) fun snap => do
    let shim := getLocalShim snap.env
    if shim.isEmpty then return prev
    let some ls ← getLs? | return prev
    let some provider := ls.capabilities.semanticTokensProvider? | return prev
    let {tokenTypes, tokenModifiers} := provider.legend
    withFallbackResponse prev do
      let task ←
        ls.withTextDocument nullUri shim.toString "c" do
          ls.call "textDocument/semanticTokens/full" ⟨⟨nullUri⟩⟩
      mergeResponses task prev fun shimTokens leanTokens =>
        let shimEntries := decodeShimTokens shimTokens.data
          shim doc.meta.text beginPos endPos tokenTypes tokenModifiers
        let leanEntries := decodeLeanTokens leanTokens.data
        -- Stable sort the combined entries (Lean first)
        let entries := leanEntries ++ shimEntries
        let entries := entries.zip <| entries.size.fold (flip Array.push) #[]
        let entries := entries.qsort fun (a,i) (b,j) =>
          a.line < b.line ∨ (a.line = b.line ∧ (a.startChar < b.startChar ∨ (a.startChar = b.startChar ∧ i < j)))
        let entries := entries.map (·.1)
        if h : entries.size > 0 then do
          -- Filter out overlapping tokens (preferring the first)
          let entries := entries[1:].foldl (init := #[entries[0]]) fun es b =>
            let a := es.back
            if a.line = b.line && (b.startChar ≤ a.startChar + a.length) then
              es
            else
              es.push b
          return {leanTokens with data := encodeTokenEntries entries}
        else
          return leanTokens

def handleSemanticTokensFull
(_ : SemanticTokensParams) (prev : RequestTask SemanticTokens)
: RequestM (RequestTask SemanticTokens) := do
  handleSemanticTokens 0 ⟨1 <<< 31⟩ prev

def handleSemanticTokensRange
(p : SemanticTokensRangeParams) (prev : RequestTask SemanticTokens)
: RequestM (RequestTask SemanticTokens) := do
  let doc ← readDoc
  let text := doc.meta.text
  let beginPos := text.lspPosToUtf8Pos p.range.start
  let endPos := text.lspPosToUtf8Pos p.range.end
  handleSemanticTokens beginPos endPos prev
