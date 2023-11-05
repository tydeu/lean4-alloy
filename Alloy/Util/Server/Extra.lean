/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Shim
import Lean.Server.Requests

open Lean Elab Server JsonRpc RequestM

namespace Alloy

@[inline] def lspRangeToUtf8Range (text : FileMap) (range : Lsp.Range) : String.Range :=
  ⟨text.lspPosToUtf8Pos range.start, text.lspPosToUtf8Pos range.end⟩

@[inline] def utf8RangeToLspRange (text : FileMap) (range : String.Range) : Lsp.Range :=
  ⟨text.utf8PosToLspPos range.start, text.utf8PosToLspPos range.stop⟩

/- Shim Position <-> Lean Mapping via Info Tree -/

partial def findShimInfo? (tree : InfoTree)
(f : Syntax → String.Range → PersistentArray InfoTree → Option α) : Option α :=
  go tree
where
  go
  | .context _ t => go t
  | .node i children =>
    if let some (stx, r) := toShimInfo? i then
      f stx r children
    else
      children.findSome? go
  | .hole .. => none

partial def findShimInfoLeaf? (tree : InfoTree)
(f : Syntax → String.Range → Option α) : Option α :=
  go tree
where
  go
  | .context _ t => go t
  | .node i children => do
    if children.isEmpty then
      let (stx, r) ← toShimInfo? i
      f stx r
    else
      children.findSome? go
  | .hole .. => none

def findShimPos? (tree : InfoTree) (leanPos : String.Pos) (includeStop := false) : Option String.Pos :=
  findShimInfoLeaf? tree fun stx shimRange => do
    let leanRange ← stx.getRange? --(canonicalOnly := true)
    guard <| leanRange.contains leanPos includeStop
    let leanOff := leanPos - leanRange.start
    let shimLen := shimRange.stop - shimRange.start
    let shimPos := shimRange.start + ⟨min leanOff.byteIdx shimLen.byteIdx⟩
    return shimPos

def findShimStx? (tree : InfoTree) (shimPos : String.Pos) (includeStop := false) : Option Syntax :=
  findShimInfoLeaf? tree fun stx shimRange => do
    guard <| shimRange.contains shimPos includeStop
    return stx

def shimRangeToLeanRange? (tree : InfoTree) (shimRange : String.Range) : Option String.Range := do
  let leanHead ← findShimStx? tree shimRange.start >>= (·.getPos?)
  let leanTail ← findShimStx? tree shimRange.stop (includeStop := true) >>= (·.getTailPos?)
  return ⟨leanHead, leanTail⟩

def shimLspRangeToLeanLsp? (shim : Shim) (leanText : FileMap) (tree : InfoTree) (shimRange : Lsp.Range)  : Option Lsp.Range := do
  let r ← shimRangeToLeanRange? tree (lspRangeToUtf8Range shim.text shimRange)
  return utf8RangeToLspRange leanText r

/- Shim Position <-> Lean Mapping via Shim Commands -/

def Shim.leanPosToLsp? (self : Shim) (leanPos : String.Pos) (includeStop := false) : Option Lsp.Position := do
  self.text.utf8PosToLspPos (← self.leanPosToShim? leanPos includeStop)

def Shim.utf8RangeToLean? (self : Shim) (shimRange : String.Range) : Option String.Range := do
  let leanHead ← self.shimPosToLeanStx? shimRange.start >>= (·.getPos?)
  let leanTail ← self.shimPosToLeanStx? shimRange.stop (includeStop := true) >>= (·.getTailPos?)
  return ⟨leanHead, leanTail⟩

def Shim.lspRangeToLeanLsp? (self : Shim) (shimRange : Lsp.Range) (leanText : FileMap) : Option Lsp.Range := do
  let r ← self.utf8RangeToLean? (lspRangeToUtf8Range self.text shimRange)
  return utf8RangeToLspRange leanText r

/- Other Utilities -/

/-- Fallback to returning `resp` if `act` errors. Also, log the error message. -/
def withFallbackResponse (resp : RequestTask α) (act : RequestM (RequestTask α)) : RequestM (RequestTask α) :=
  try
    act
  catch e =>
    (←read).hLog.putStrLn s!"Shim language server request failed: {e.message}"
    return resp

def shimRequestError [ToString α] : ResponseError α → RequestError
| {id, code, message, data?} =>
  let data := data?.map (s!"\n{·}") |>.getD ""
  .mk code s!"Shim language server request {id} failed: {message}{data}"

def mergeResponses (shimTask : Task (Except (ResponseError Json) α))
(leanTask : RequestTask α) (f : α → α → RequestM α) : RequestM (RequestTask α) := do
  bindTask shimTask fun
  | .ok shimResult => do
    bindTask leanTask fun
    | .ok leanResult =>
      return Task.pure <| .ok <| ← f shimResult leanResult
    | .error e => throw e
  | .error e => throw <| shimRequestError e
