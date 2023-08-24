/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Shim
import Lean.Server.Requests

open Lean Server JsonRpc RequestM

namespace Alloy

def Shim.leanPosToLsp? (self : Shim) (leanPos : String.Pos) : Option Lsp.Position := do
  self.text.utf8PosToLspPos (← self.leanPosToShim? leanPos)

def Shim.lspPosToLean? (self : Shim) (shimPos : Lsp.Position) : Option String.Pos := do
  self.shimPosToLean? (self.text.lspPosToUtf8Pos shimPos)

def Shim.posToLeanLsp? (self : Shim) (shimPos : String.Pos) (leanText : FileMap) : Option Lsp.Position := do
  leanText.utf8PosToLspPos (← self.shimPosToLean? shimPos)

def Shim.lspPosToLeanLsp? (self : Shim) (shimPos : Lsp.Position) (leanText : FileMap) : Option Lsp.Position := do
  leanText.utf8PosToLspPos (← self.lspPosToLean? shimPos)

def Shim.lspRangeToLeanLsp? (self : Shim) (shimRange : Lsp.Range) (leanText : FileMap) : Option Lsp.Range := do
  let startPos ← self.lspPosToLeanLsp? shimRange.start leanText
  let beforeEndPos := self.text.source.prev (self.text.lspPosToUtf8Pos shimRange.end)
  let beforeEndPos := self.text.source.next (← self.shimPosToLean? beforeEndPos)
  return ⟨startPos, leanText.utf8PosToLspPos beforeEndPos⟩

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
