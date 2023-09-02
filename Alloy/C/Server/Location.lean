/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Shim
import Alloy.C.Server.Worker
import Alloy.Util.Server

open Lean Server Lsp RequestM JsonRpc

namespace Alloy.C

def handleLocation (p : Lsp.Position)
(method : String) [LsCall method TextDocumentPositionParams α]
 (prev : RequestTask α) (f : Shim → α → α → RequestM α) : RequestM (RequestTask α) := do
  let doc ← readDoc
  let text := doc.meta.text
  let leanHoverPos := text.lspPosToUtf8Pos p
  bindWaitFindSnap doc (·.endPos > leanHoverPos) (notFoundX := pure prev) fun snap => do
    let shim := getLocalShim snap.env
    let some shimHoverPos := shim.leanPosToLsp? leanHoverPos | return prev
    let some ls ← getLs? | return prev
    withFallbackResponse prev do
      let task ← do
        ls.withTextDocument nullUri shim.toString "c" do
          ls.call method {
            textDocument := ⟨nullUri⟩,
            position := shimHoverPos
          }
      mergeResponses task prev (f shim)

/-! ## Completion Support -/

def handleCompletion (p : CompletionParams)
(prev : RequestTask CompletionList) : RequestM (RequestTask CompletionList) := do
  let doc ← readDoc
  let text := doc.meta.text
  let cursorPos := text.lspPosToUtf8Pos p.position
  -- work around https://github.com/microsoft/vscode/issues/155738
  let abortedX := pure <| Task.pure <| .ok { items := #[{label := "-"}], isIncomplete := true }
  bindWaitFindSnap doc (·.endPos >= cursorPos) (notFoundX := pure prev) (abortedX := abortedX) fun snap => do
    let shim := getLocalShim snap.env
    let prevCharPos := text.source.prev cursorPos
    let some shimCharPos := shim.leanPosToShim? prevCharPos | return prev
    let shimCursorPos := shim.text.utf8PosToLspPos <| shim.text.source.next shimCharPos
    let some ls ← getLs? | return prev
    withFallbackResponse prev do
      let task ← do
        let ready ← IO.Promise.new
        ls.withNotificationHandler "textDocument/clangd.fileStatus"
          (fun {state,..} => do if state = "idle" then ready.resolve ()) do
          ls.withTextDocument nullUri shim.toString "c" do
            IO.wait ready.result
            ls.call "textDocument/completion" {
              textDocument := ⟨nullUri⟩,
              position := shimCursorPos
            }
      mergeResponses task prev fun shimResult leanResult =>
        let trRange? range := shim.lspRangeToLeanLsp? range text
        let shimItems := shimResult.items.map fun item =>
          {item with
            textEdit? := item.textEdit?.bind fun edit =>
              return {edit with insert := ← trRange? edit.insert, replace := ← trRange? edit.replace}
            additionalTextEdits? := item.additionalTextEdits?.bind fun a => a.filterMap fun edit =>
              return {edit with range := ← trRange? edit.range}
            -- NOTE: clangd mistakenly marks identifiers as deprecated
            deprecated? := if item.kind? = some .text then none else item.deprecated?
          }
        return {
          isIncomplete := shimResult.isIncomplete || leanResult.isIncomplete
          items := shimItems ++ leanResult.items
        }

/-! ## Hover Support -/

def handleHover (p : HoverParams)
(prev : RequestTask (Option Hover)) : RequestM (RequestTask (Option Hover)) := do
  have : LsCall "textDocument/hover" TextDocumentPositionParams (Option Hover) := {}
  handleLocation p.position "textDocument/hover" prev fun shim shimHover? leanHover? => do
    let some shimHover := shimHover? | return leanHover?
    let text := (← readDoc).meta.text
    let range? := shimHover.range? >>= (shim.lspRangeToLeanLsp? · text)
    if let some leanHover := leanHover? then
      let v := s!"{leanHover.contents.value}\n\n---\n\n{shimHover.contents.value}"
      -- We use the shim range here because it is generally not too big or too small
      return some {leanHover with contents.value := v, range?}
    else
      return some {shimHover with range?}

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
        if let some range := shim.lspRangeToLeanLsp? link.targetRange text then
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
