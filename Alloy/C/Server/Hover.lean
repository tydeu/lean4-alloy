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
