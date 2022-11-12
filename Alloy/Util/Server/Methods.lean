/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.Util.Server.Initialize

open Lean Lsp

namespace Alloy

/-- An LSP notification method. -/
class LsNote (method : String) (α : outParam $ Type u)
  extends ToJson α

@[default_instance low] instance : LsNote m Json := {}
instance : LsNote "initialized" InitializedParams := {}
instance : LsNote "textDocument/didOpen" DidOpenTextDocumentParams := {}
instance : LsNote "textDocument/didClose" DidCloseTextDocumentParams := {}

/-- An LSP request/response method. -/
class LsCall (method : String) (α : outParam $ Type u) (β : outParam $ Type w)
  extends ToJson α, FromJson β

@[default_instance low] instance : LsCall m Json Json := {}
instance : LsCall "initialize" InitializeParams InitializeResult := {}
instance : LsCall "textDocument/hover" HoverParams (Option Hover) := {}
instance : LsCall "textDocument/semanticTokens/full"  SemanticTokensParams SemanticTokens := {}
instance : LsCall "textDocument/semanticTokens/range" SemanticTokensRangeParams SemanticTokens := {}

def locationToLink : Location → LocationLink
| {uri, range} => {
    originSelectionRange? := none
    targetUri := uri
    targetRange := range
    targetSelectionRange := range
}

/-- A location result can be `Location | Location[] | LocationLink[] | null`. -/
def resultToLinks? : Json → Except String (Array (LocationLink))
| .null => .ok #[]
| .arr js => Id.run <| ExceptT.run <| js.mapM fun j =>
  try locationToLink <$> fromJson? j catch _ => fromJson? j
| j@(.obj ..) => Array.singleton <$> locationToLink <$> fromJson? j
| _ => .error "null, object, or array expected"

instance : LsCall "textDocument/declaration" TextDocumentPositionParams (Array LocationLink) := {fromJson? := resultToLinks?}
instance : LsCall "textDocument/definition" TextDocumentPositionParams (Array LocationLink) := {fromJson? := resultToLinks?}
instance : LsCall "textDocument/typeDefinition" TextDocumentPositionParams (Array LocationLink) := {fromJson? := resultToLinks?}
