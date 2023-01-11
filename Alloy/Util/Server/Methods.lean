/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.Util.Server.Initialize

open Lean Lsp

namespace Alloy

/-- An LSP server to client notification method. -/
class LsClientNote (method : String) (α : outParam $ Type u)
  extends FromJson α

@[default_instance low] instance : LsClientNote m Json := {}

/-- An LSP client to server notification method. -/
class LsServerNote (method : String) (α : outParam $ Type u)
  extends ToJson α

@[default_instance low] instance : LsServerNote m Json := {}
instance : LsServerNote "initialized" InitializedParams := {}
instance : LsServerNote "textDocument/didOpen" DidOpenTextDocumentParams := {}
instance : LsServerNote "textDocument/didClose" DidCloseTextDocumentParams := {}

/-- An LSP client to server request/response method. -/
class LsCall (method : String) (α : outParam $ Type u) (β : outParam $ Type w)
  extends ToJson α, FromJson β

@[default_instance low] instance : LsCall m Json Json := {}
instance : LsCall "initialize" InitializeParams InitializeResult := {}
instance : LsCall "textDocument/hover" HoverParams (Option Hover) := {}
instance : LsCall "textDocument/semanticTokens/full"  SemanticTokensParams SemanticTokens := {}
instance : LsCall "textDocument/semanticTokens/range" SemanticTokensRangeParams SemanticTokens := {}

structure TextEdit where
  range : Range
  newText : String
  deriving ToJson, FromJson

/-- A completion edit can be `TextEdit | InsertReplaceEdit`. -/
def CompletionTextEdit := InsertReplaceEdit

instance : ToJson CompletionTextEdit := inferInstanceAs (ToJson InsertReplaceEdit)
instance : FromJson CompletionTextEdit where
  fromJson? j :=
    try textToInsertReplace <$> fromJson? j catch _ => fromJson? (α := InsertReplaceEdit) j
where
  textToInsertReplace : TextEdit → InsertReplaceEdit
  | {range, newText} => {newText, insert := range, replace := range}

inductive InsertTextFormat where
| plainText
| snippet
deriving DecidableEq

instance : ToJson InsertTextFormat where
 toJson a  := toJson <| a.toCtorIdx + 1

instance : FromJson InsertTextFormat where
 fromJson? v :=  return .ofNat ((← fromJson? v)-1)

structure CompletionItemLabelDetails where
  details? : Option String := none
  description? : Option String := none
  deriving FromJson, ToJson

/-- An LSP [CompletionItem](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItem). -/
structure CompletionItem (Data := Json) where
  label : String
  labelDetails? : Option CompletionItemLabelDetails := none
  kind? : Option CompletionItemKind := none
  tags? : Option (Array CompletionItemTag) := none
  detail? : Option String := none
  documentation? : Option MarkupContent := none
  deprecated? : Option Bool := none
  preselect? : Option Bool := none
  sortText? : Option String := none
  filterText? : Option String := none
  insertText? : Option String := none
  insertTextFormat? : Option InsertTextFormat := none
  insertTextMode? : Option InsertTextMode := none
  textEdit? : Option CompletionTextEdit := none
  additionalTextEdits? : Option (Array TextEdit) := none
  commitCharacters? : Option (Array String) := none
  command? : Option Lsp.Command := none
  data? : Option Data := none
  deriving FromJson, ToJson, Inhabited

structure EditRange where
  insert : Range
  replace : Range
  deriving FromJson, ToJson

/-- `EditRange` can be `Range | EditRange`. -/
instance : FromJson EditRange where
  fromJson? j := try (fun r => ⟨r,r⟩) <$> fromJson? j catch _ => fromJson? j

structure CompletionItemDefault (Data := Json) where
  commitCharacters? : Option (Array String) := none
  editRange? : Option EditRange := none
  insertTextFormat? : Option InsertTextFormat := none
  insertTextMode? : Option InsertTextMode := none
  data? : Option Data := none
  deriving FromJson, ToJson

/--
An LSP [CompletionList](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionList).
Represents a collection of completion items to be presented in the editor.
-/
structure CompletionList (Data := Json) where
  isIncomplete : Bool := false
  itemDefaults? : Option (Array (CompletionItemDefault Data)) := none
  items : Array (CompletionItem Data) := #[]
  deriving FromJson, ToJson

/-- A completion result can be `CompletionItem[] | CompletionList | null`. -/
instance : LsCall "textDocument/completion" CompletionParams CompletionList where
  fromJson?
    | .null => .ok {}
    | .arr js => return {items := ← js.mapM fromJson?}
    | j@(.obj ..) => fromJson? j
    | _ => .error "null, object, or array expected"

/-- A location result can be `Location | Location[] | LocationLink[] | null`. -/
def resultToLinks? : Json → Except String (Array LocationLink)
| .null => .ok #[]
| .arr js => js.mapM fun j =>
  try locationToLink <$> fromJson? j catch _ => fromJson? j
| j@(.obj ..) => Array.singleton <$> locationToLink <$> fromJson? j
| _ => .error "null, object, or array expected"
where
  locationToLink : Location → LocationLink
  | {uri, range} => {
      originSelectionRange? := none
      targetUri := uri
      targetRange := range
      targetSelectionRange := range
  }

instance : LsCall "textDocument/declaration" TextDocumentPositionParams (Array LocationLink) := {fromJson? := resultToLinks?}
instance : LsCall "textDocument/definition" TextDocumentPositionParams (Array LocationLink) := {fromJson? := resultToLinks?}
instance : LsCall "textDocument/typeDefinition" TextDocumentPositionParams (Array LocationLink) := {fromJson? := resultToLinks?}
