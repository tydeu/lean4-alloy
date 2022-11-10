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

/-! ## Hover Support -/

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
      let task ←
        ls.withTextDocument nullUri shim.toString "c" do
          ls.call "textDocument/hover" {
            textDocument := ⟨nullUri⟩,
            position := hoverCPos
          }
      bindTask task fun
      | .ok cHover? => do
        let some cHover := cHover? | return prev
        bindTask prev fun leanHover? => do
          if let some leanHover := leanHover?.toOption.bind (·) then
            let v := s!"{leanHover.contents.value}\n\n---\n\n{cHover.contents.value}"
            return Task.pure <| .ok <| some {leanHover with contents.value := v}
          else
            return Task.pure <| .ok <| some {cHover with range? := none}
      | .error e => throw <| cRequestError e
