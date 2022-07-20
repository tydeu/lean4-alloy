/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Extension
import Alloy.C.Server.Worker
import Alloy.C.Server.Utils
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
