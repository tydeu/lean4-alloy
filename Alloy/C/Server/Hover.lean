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

def handleLocation (p : Lsp.Position)
(method : String) [LsCall method TextDocumentPositionParams α]
 (prev : RequestTask α) (f : Shim → α → α → RequestM α) : RequestM (RequestTask α) := do
  let doc ← readDoc
  let text := doc.meta.text
  let hoverPos := text.lspPosToUtf8Pos p
  bindWaitFindSnap doc (·.endPos > hoverPos) (notFoundX := pure prev) fun snap => do
    let shim := getLocalShim snap.env
    let some hoverCPos := shim.leanPosToCLsp? hoverPos | return prev
    let some ls ← getLs? | return prev
    withFallbackResponse prev do
      let task ←
        ls.withTextDocument nullUri shim.toString "c" do
          ls.call method {
            textDocument := ⟨nullUri⟩,
            position := hoverCPos
          }
      bindTask task fun
      | .ok shimResult =>
        bindTask prev fun
        | .ok leanResult =>
          return Task.pure <| .ok <| ← f shim shimResult leanResult
        | .error e => throw e
      | .error e => throw <| cRequestError e


/-! ## Hover Support -/

def handleHover
(p : HoverParams) (prev : RequestTask (Option Hover))
: RequestM (RequestTask (Option Hover)) := do
  have : LsCall "textDocument/hover" TextDocumentPositionParams (Option Hover) := {}
  handleLocation p.position "textDocument/hover" prev fun _ shimHover? leanHover? => do
    let some shimHover := shimHover? | return leanHover?
    if let some leanHover := leanHover? then
      let v := s!"{leanHover.contents.value}\n\n---\n\n{shimHover.contents.value}"
      return some {leanHover with contents.value := v}
    else
      return some {shimHover with range? := none}

/-! ## Goto Support -/

def handleGoto
(method : String) [LsCall method TextDocumentPositionParams (Array LocationLink)]
(p : TextDocumentPositionParams) (prev : RequestTask (Array LocationLink))
: RequestM (RequestTask (Array LocationLink)) := do
  handleLocation p.position method prev fun shim shimLinks leanLinks => do
    let text := (← readDoc).meta.text
    let originSelectionRange? := leanLinks.findSome? (·.originSelectionRange?)
    let shimLinks := shimLinks.filterMap fun link =>
      if isNullUri link.targetUri then
        if let some range := shim.cLspRangeToLeanLsp? link.targetRange text then
          some {link with
            originSelectionRange?
            targetUri := p.textDocument.uri
            targetRange := range, targetSelectionRange := range
          }
        else
          none
      else
        some {link with originSelectionRange?}
    return if shimLinks.isEmpty then leanLinks else shimLinks
