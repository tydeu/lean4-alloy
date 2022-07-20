/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Data.Lsp.Capabilities

open Lean Lsp

namespace Alloy

structure EmptyObject deriving ToJson, FromJson
instance : EmptyCollection EmptyObject := ⟨.mk⟩

def Union := Sum

def Union.inl (a : α) : Union α β := Sum.inl a
def Union.inr (b : β) : Union α β := Sum.inr b

instance : Coe α (Union α β) := ⟨.inl⟩
instance : Coe β (Union α β) := ⟨.inr⟩

instance [ToJson α] [ToJson β] : ToJson (Union α β) where
  toJson | .inl a => toJson a | .inr b => toJson b

instance [FromJson α] [FromJson β] : FromJson (Union α β) where
  fromJson? v :=
    match fromJson? v with
    | .ok a => .ok <| .inl a
    | .error _ => .inr <$> fromJson? v

abbrev BUnion (α) := Union Bool α
abbrev BOption (α) := Option (BUnion α)

structure WorkDoneProgressOptions where
  workDoneProgress? : Option Bool := none

deriving instance DecidableEq for SymbolKind

instance : FromJson SymbolKind where
 fromJson? v := do
    let i : Nat ← fromJson? v
    return SymbolKind.ofNat (i-1)

deriving instance DecidableEq for SymbolTag

instance : FromJson SymbolTag where
  fromJson? v := return .ofNat ((← fromJson? v)-1)

inductive CompletionItemTag where
| deprecated
deriving Inhabited, DecidableEq, Repr

instance : ToJson CompletionItemTag where
  toJson a := toJson <| a.toCtorIdx + 1

instance : FromJson CompletionItemTag where
  fromJson? v := return .ofNat ((← fromJson? v)-1)

inductive InsertTextMode where
| asIs | adjustIndentation
deriving Inhabited, DecidableEq, Repr

instance : ToJson InsertTextMode where
  toJson a := toJson <| a.toCtorIdx + 1

instance : FromJson InsertTextMode where
  fromJson? v := return .ofNat ((← fromJson? v)-1)

inductive PrepareSupportDefaultBehavior  where
| identifier
deriving Inhabited, DecidableEq, Repr

instance : ToJson PrepareSupportDefaultBehavior where
  toJson a := toJson <| a.toCtorIdx + 1

instance : FromJson PrepareSupportDefaultBehavior where
  fromJson? v := return .ofNat ((← fromJson? v)-1)

inductive DiagnosticTag  where
| unnecessary | deprecated
deriving Inhabited, DecidableEq, Repr

instance : ToJson DiagnosticTag where
  toJson a := toJson <| a.toCtorIdx + 1

instance : FromJson DiagnosticTag where
  fromJson? v := return .ofNat ((← fromJson? v)-1)

deriving instance FromJson for FoldingRangeKind

---

inductive ResourceOperationKind
| create | rename | delete
deriving ToJson, FromJson

inductive FailureHandlingKind
| abort | transactional | undo | textOnlyTransactional
deriving ToJson, FromJson

structure ChangeAnnotationSupportClientCapabilities where
  groupsOnLabel? : Option Bool := none
  deriving ToJson, FromJson

structure WorkspaceEditClientCapabilities where
  documentChanges? : Option Bool := none
  resourceOperations? : Option (Array ResourceOperationKind) := none
  failureHandling? : Option FailureHandlingKind := none
  normalizeLineEndings? : Option Bool := none
  changeAnnotationSupport? : Option ChangeAnnotationSupportClientCapabilities := none
  deriving ToJson, FromJson

structure DidChangeConfigurationClientCapabilities where
  dynamicRegistration? : Option Bool := none
  deriving ToJson, FromJson

structure DidChangeWatchedFilesClientCapabilities where
  dynamicRegistration? : Option Bool := none
  relativePatternSupport? : Option Bool := none
  deriving ToJson, FromJson

structure SymbolKindCapabilities where
  valueSet? : Option (Array SymbolKind) := none
  deriving ToJson, FromJson

structure SymbolTagSupportCapabilities where
  valueSet : Array SymbolTag
  deriving ToJson, FromJson

structure ResolveSupportCapabilities where
  properties : Array String
  deriving ToJson, FromJson

structure WorkspaceSymbolClientCapabilities where
  dynamicRegistration? : Option Bool := none
  symbolKind? : Option SymbolKindCapabilities := none
  tagSupport? : Option SymbolTagSupportCapabilities := none
  resolveSupport? : Option ResolveSupportCapabilities := none
  deriving ToJson, FromJson

structure ExecuteCommandClientCapabilities extends WorkDoneProgressOptions where
  commands : Array String
  deriving ToJson, FromJson

structure SemanticTokensWorkspaceClientCapabilities where
  refreshSupport? : Option Bool := none
  deriving ToJson, FromJson

structure CodeLensWorkspaceClientCapabilities where
  refreshSupport? : Option Bool := none
  deriving ToJson, FromJson

structure FileOperationsCapabilities where
  dynamicRegistration? : Option Bool := none
  didCreate? : Option Bool := none
  willCreate? : Option Bool := none
  didRename? : Option Bool := none
  willRename? : Option Bool := none
  didDelete? : Option Bool := none
  willDelete? : Option Bool := none
  deriving ToJson, FromJson

structure InlineValueWorkspaceClientCapabilities where
  refreshSupport? : Option Bool := none
  deriving ToJson, FromJson

structure InlayHintWorkspaceClientCapabilities where
  refreshSupport? : Option Bool := none
  deriving ToJson, FromJson

structure DiagnosticWorkspaceClientCapabilities where
  refreshSupport? : Option Bool := none
  deriving ToJson, FromJson

structure WorkspaceClientCapabilities where
  applyEdit? : Option Bool := none
  workspaceEdit? : Option WorkspaceEditClientCapabilities := none
  didChangeConfiguration? : Option DidChangeConfigurationClientCapabilities := none
  didChangeWatchedFiles? : Option DidChangeWatchedFilesClientCapabilities := none
  symbol? : Option WorkspaceSymbolClientCapabilities := none
  executeCommand? : Option ExecuteCommandClientCapabilities := none
  workspaceFolders? : Option Bool := none
  configuration? : Option Bool := none
  semanticTokens? : Option SemanticTokensWorkspaceClientCapabilities := none
  codeLens? : Option CodeLensWorkspaceClientCapabilities := none
  fileOperations? : Option FileOperationsCapabilities := none
  inlineValue? : Option InlineValueWorkspaceClientCapabilities := none
  inlayHint? : Option InlayHintWorkspaceClientCapabilities := none
  diagnostics? : Option DiagnosticWorkspaceClientCapabilities := none
  deriving ToJson, FromJson

structure TextDocumentSyncClientCapabilities where
  dynamicRegistration? : Option Bool := none
  willSave? : Option Bool := none
  willSaveWaitUntil? : Option Bool := none
  didSave? : Option Bool := none
  deriving ToJson, FromJson

structure CompletionItemTagSupportCapabilities where
  valueSet : Array CompletionItemTag
  deriving ToJson, FromJson

structure InsertTextModeSupportCapabilities where
  valueSet : Array InsertTextMode
  deriving ToJson, FromJson

structure CompletionItemCapabilities where
  snippetSupport? : Option Bool := none
  commitCharactersSupport? : Option Bool := none
  documentationFormat? : Option (Array MarkupKind) := none
  deprecatedSupport? : Option Bool := none
  preselectSupport? : Option Bool := none
  tagSupport? : Option CompletionItemTagSupportCapabilities := none
  insertReplaceSupport? : Option Bool := none
  insertTextModeSupport? : Option InsertTextModeSupportCapabilities := none
  resolveSupport? : Option ResolveSupportCapabilities := none
  labelDetailsSupport? : Option Bool := none
  deriving ToJson, FromJson

structure CompletionItemKindCapabilities where
  valueSet? : Option (Array CompletionItemKind) := none
  deriving ToJson, FromJson

structure CompletionListCapabilities where
  itemDefaults? : Option (Array String) := none
  deriving ToJson, FromJson

structure CompletionClientCapabilities where
  dynamicRegistration? : Option Bool := none
  completionItem? : Option CompletionItemCapabilities := none
  completionItemKind? : Option CompletionItemKindCapabilities := none
  contextSupport? : Option Bool := none
  insertTextMode? : Option InsertTextMode := none
  completionList? : Option CompletionListCapabilities := none
  deriving ToJson, FromJson

structure HoverClientCapabilities where
  dynamicRegistration? : Option Bool := none
  contentFormat? : Option (Array MarkupKind) := none
  deriving ToJson, FromJson

structure ParameterInformationCapabilities where
  labelOffsetSupport? : Option Bool := none
  deriving ToJson, FromJson

structure SignatureInformationCapabilities where
  documentationFormat? : Option (Array MarkupKind) := none
  parameterInformation? : Option ParameterInformationCapabilities := none
  activeParameterSupport : Option Bool := none
  deriving ToJson, FromJson

structure SignatureHelpClientCapabilities where
  dynamicRegistration? : Option Bool := none
  signatureInformation? : Option SignatureInformationCapabilities := none
  contentSupport? : Option Bool := none
  deriving ToJson, FromJson

structure DeclarationClientCapabilities where
  dynamicRegistration? : Option Bool := none
  linkSupport? : Option Bool := none
  deriving ToJson, FromJson

structure DefinitionClientCapabilities where
  dynamicRegistration? : Option Bool := none
  linkSupport? : Option Bool := none
  deriving ToJson, FromJson

structure TypeDefinitionClientCapabilities where
  dynamicRegistration? : Option Bool := none
  linkSupport? : Option Bool := none
  deriving ToJson, FromJson

structure ImplementationClientCapabilities where
  dynamicRegistration? : Option Bool := none
  linkSupport? : Option Bool := none
  deriving ToJson, FromJson

structure ReferenceClientCapabilities where
  dynamicRegistration? : Option Bool := none
  deriving ToJson, FromJson

structure DocumentHighlightClientCapabilities where
  dynamicRegistration? : Option Bool := none
  deriving ToJson, FromJson

structure DocumentSymbolClientCapabilities where
  dynamicRegistration? : Option Bool := none
  symbolKind? : Option SymbolKindCapabilities := none
  hierarchicalDocumentSymbolSupport? : Option Bool := none
  tagSupport? : Option SymbolTagSupportCapabilities := none
  labelSupport? : Option Bool := none
  deriving ToJson, FromJson

def CodeActionKind := Name

namespace CodeActionKind

def empty := Name.anonymous
def quickfix := `quickfix
def refactor := `refactor
def refactorExtract := `refactor.extract
def refactorInline := `refactor.inline
def refactorRewrite := `refactor.inline
def source := `refactor.inline
def sourceOrganizeImports := `source.organizeImports
def sourceFixAll := `source.fixAll

instance : ToJson CodeActionKind where
  toJson a := if a.isAnonymous then "" else a.toString false

instance : FromJson CodeActionKind where
  fromJson? v := v.getStr? <&> fun v => if v.isEmpty then empty else v.toName

end CodeActionKind

structure CodeActionKindCapabilities where
  valueSet : Array CodeActionKind
  deriving ToJson, FromJson

structure CodeActionLiteralSupportCapabilities where
  codeActionKind : CodeActionKindCapabilities
  deriving ToJson, FromJson

structure CodeActionClientCapabilities where
  dynamicRegistration? : Option Bool := none
  codeActionLiteralSupport? : Option CodeActionLiteralSupportCapabilities := none
  isPreferredSupport? : Option Bool := none
  disabledSupport? : Option Bool := none
  dataSupport? : Option Bool := none
  resolveSupport? : Option ResolveSupportCapabilities := none
  honorsChangeAnnotations? : Option Bool := none
  deriving ToJson, FromJson

structure CodeLensClientCapabilities where
  dynamicRegistration? : Option Bool := none
  deriving ToJson, FromJson

structure DocumentLinkClientCapabilities where
  dynamicRegistration? : Option Bool := none
  tooltipSupport? : Option Bool := none
  deriving ToJson, FromJson

structure DocumentColorClientCapabilities where
  dynamicRegistration? : Option Bool := none
  deriving ToJson, FromJson

structure DocumentFormattingClientCapabilities where
  dynamicRegistration? : Option Bool := none
  deriving ToJson, FromJson

structure DocumentRangeFormattingClientCapabilities where
  dynamicRegistration? : Option Bool := none
  deriving ToJson, FromJson

structure DocumentOnTypeFormattingClientCapabilities where
  dynamicRegistration? : Option Bool := none
  deriving ToJson, FromJson

structure RenameClientCapabilities where
  dynamicRegistration? : Option Bool := none
  prepareSupport? : Option Bool := none
  prepareSupportDefaultBehavior? : Option PrepareSupportDefaultBehavior := none
  honorsChangeAnnotations? : Option Bool := none
  deriving ToJson, FromJson

structure DiagnosticTagSupportCapabilities where
  valueSet : Array DiagnosticTag
  deriving ToJson, FromJson

structure PublishDiagnosticsClientCapabilities where
  relatedInformation? : Option Bool := none
  tagSupport? : Option DiagnosticTagSupportCapabilities := none
  versionSupport? : Option PrepareSupportDefaultBehavior := none
  codeDescriptionSupport? : Option Bool := none
  dataSupport? : Option Bool := none
  deriving ToJson, FromJson

structure FoldingRangeKindCapabilities where
  valueSet? : Option (Array FoldingRangeKind) := none
  deriving ToJson, FromJson

structure FoldingRangeCapabilities where
  collapsedText? : Option Bool := none
  deriving ToJson, FromJson

structure FoldingRangeClientCapabilities where
  dynamicRegistration? : Option Bool := none
  rangeLimit? : Option Nat := none
  lineFoldingOnly? : Option Bool := none
  foldingRangeKind? : Option FoldingRangeKindCapabilities := none
  foldingRange? : Option FoldingRangeCapabilities := none
  deriving ToJson, FromJson

structure SelectionRangeClientCapabilities where
  dynamicRegistration? : Option Bool := none
  deriving ToJson, FromJson

structure LinkedEditingRangeClientCapabilities where
  dynamicRegistration? : Option Bool := none
  deriving ToJson, FromJson

structure CallHierarchyClientCapabilities where
  dynamicRegistration? : Option Bool := none
  deriving ToJson, FromJson

structure SemanticTokensFullCapabilities where
  delta? : Option Bool := none
  deriving ToJson, FromJson

structure SemanticTokensRequestsCapabilities where
  range? : BOption EmptyObject := none
  full? : BOption SemanticTokensFullCapabilities := none
  deriving ToJson, FromJson

inductive TokenFormat
| relative
deriving ToJson, FromJson

structure SemanticTokensClientCapabilities where
  dynamicRegistration? : Option Bool := none
  requests : SemanticTokensRequestsCapabilities
  tokenTypes : Array String
  tokenModifiers : Array String
  formats : Array TokenFormat
  overlappingTokenSupport? : Option Bool := none
  multilineTokenSupport? : Option Bool := none
  serverCancelSupport? : Option Bool := none
  augmentsSyntaxTokens : Option Bool := none
  deriving ToJson, FromJson

structure MonikerClientCapabilities where
  dynamicRegistration? : Option Bool := none
  deriving ToJson, FromJson

structure TypeHierarchyClientCapabilities where
  dynamicRegistration? : Option Bool := none
  deriving ToJson, FromJson

structure InlineValueClientCapabilities where
  dynamicRegistration? : Option Bool := none
  deriving ToJson, FromJson

structure InlayHintClientCapabilities where
  dynamicRegistration? : Option Bool := none
  resolveSupport? : Option ResolveSupportCapabilities := none
  deriving ToJson, FromJson

structure DiagnosticClientCapabilities where
  dynamicRegistration? : Option Bool := none
  relatedDocumentSupport? : Option Bool := none
  deriving ToJson, FromJson

structure TextDocumentClientCapabilities where
  synchronization? : Option TextDocumentSyncClientCapabilities := none
  completion? : Option CompletionClientCapabilities := none
  hover? : Option HoverClientCapabilities := none
  signatureHelp? : Option SignatureHelpClientCapabilities := none
  declaration? : Option DeclarationClientCapabilities := none
  definition? : Option DefinitionClientCapabilities := none
  typeDefinition? : Option TypeDefinitionClientCapabilities := none
  implementation? : Option ImplementationClientCapabilities := none
  references? : Option ReferenceClientCapabilities := none
  documentHighlight? : Option DocumentHighlightClientCapabilities := none
  documentSymbol? : Option DocumentSymbolClientCapabilities := none
  codeAction? : Option CodeActionClientCapabilities := none
  codeLens? : Option CodeLensClientCapabilities := none
  documentLink? : Option DocumentLinkClientCapabilities := none
  colorProvider? : Option DocumentColorClientCapabilities := none
  formatting? : Option DocumentFormattingClientCapabilities := none
  rangeFormatting? : Option DocumentRangeFormattingClientCapabilities := none
  onTypeFormatting? : Option DocumentOnTypeFormattingClientCapabilities := none
  rename? : Option RenameClientCapabilities := none
  publishDiagnostics? : Option PublishDiagnosticsClientCapabilities := none
  foldingRange? : Option FoldingRangeClientCapabilities := none
  selectionRange? : Option SelectionRangeClientCapabilities := none
  linkedEditingRange? : Option LinkedEditingRangeClientCapabilities := none
  callHierarchy? : Option CallHierarchyClientCapabilities := none
  semanticTokens? : Option SemanticTokensClientCapabilities := none
  moniker? : Option MonikerClientCapabilities := none
  typeHierarchy? : Option TypeHierarchyClientCapabilities := none
  inlineValue? : Option InlineValueClientCapabilities := none
  inlayHint? : Option InlayHintClientCapabilities := none
  diagnostic? : Option DiagnosticClientCapabilities := none
  deriving ToJson, FromJson

structure NotebookDocumentSyncClientCapabilities where
  dynamicRegistration? : Option Bool := none
  executionSummarySupport? : Option Bool := none
  deriving ToJson, FromJson

structure NotebookDocumentClientCapabilities where
  synchronization : NotebookDocumentSyncClientCapabilities
  deriving ToJson, FromJson

structure MessageActionItemCapabilities where
  additionalPropertiesSupport? : Option Bool := none
  deriving ToJson, FromJson

structure ShowMessageRequestClientCapabilities where
  messageActionItem? : Option MessageActionItemCapabilities := none
  deriving ToJson, FromJson

structure WindowClientCapabilities where
  workDoneProgress? : Option Bool := none
  showMessage? : Option ShowMessageRequestClientCapabilities := none
  showDocument? : Option ShowDocumentClientCapabilities := none
  deriving ToJson, FromJson

structure StaleRequestSupportCapabilities where
  cancel : Bool
  retryOnContentModified : Array String
  deriving ToJson, FromJson

structure RegularExpressionsClientCapabilities where
  engine : String
  version? : Option String := none
  deriving ToJson, FromJson

structure MarkdownClientCapabilities where
  parser : String
  version? : Option String := none
  allowedTags? : Option (Array String) := none
  deriving ToJson, FromJson

abbrev PositionEncodingKind := String
namespace PositionEncodingKind
def utf8 := "utf-8"
def utf16 := "utf-16"
def utf32 := "utf-32"
end PositionEncodingKind

structure GeneralClientCapabilities where
  staleRequestSupport? : Option StaleRequestSupportCapabilities := none
  regularExpressions? : Option RegularExpressionsClientCapabilities := none
  markdown? : Option MarkdownClientCapabilities := none
  positionEncodings? : Option (Array PositionEncodingKind) := none
  deriving ToJson, FromJson

structure ClientCapabilities (Experimental := Json) where
  workspace? : Option WorkspaceClientCapabilities := none
  textDocument? : Option TextDocumentClientCapabilities := none
  notebookDocument? : Option NotebookDocumentClientCapabilities := none
  window? : Option WindowClientCapabilities := none
  general? : Option GeneralClientCapabilities := none
  experimental? : Option Experimental := none
  deriving Inhabited, ToJson, FromJson

instance [ToJson Exp] : Coe (ClientCapabilities Exp) ClientCapabilities where
  coe caps := {caps with experimental? := caps.experimental?.map toJson}

---

structure TextDocumentSyncOptions where
  openClose? : Option Bool := none
  change? : Option TextDocumentSyncKind := none
  deriving ToJson, FromJson

/--
Permits parsing a `TextDocumentSyncOptions`
from a plain `TextDocumentSyncKind` (i.e., a number).
-/
instance : FromJson TextDocumentSyncOptions where
  fromJson? v := try return {change? := some <| ← fromJson? v} catch _ => fromJson? v

structure StaticRegistrationOptions where
  id? : Option String := none

structure TextDocumentRegistrationOptions where
  documentSelector : Option DocumentSelector := none

abbrev NotebookDocumentSyncRegistrationOptions := Json
abbrev CompletionOptions := Json
abbrev HoverOptions := Json
abbrev SignatureHelpOptions := Json
abbrev DeclarationRegistrationOptions := Json
abbrev DefinitionOptions := Json
abbrev TypeDefinitionRegistrationOptions := Json
abbrev ImplementationRegistrationOptions := Json
abbrev ReferenceOptions := Json
abbrev DocumentHighlightOptions := Json
abbrev DocumentSymbolOptions := Json
abbrev CodeActionOptions := Json
abbrev CodeLensOptions := Json
abbrev DocumentLinkOptions := Json
abbrev DocumentColorRegistrationOptions := Json
abbrev DocumentFormattingOptions := Json
abbrev DocumentRangeFormattingOptions := Json
abbrev DocumentOnTypeFormattingOptions := Json
abbrev RenameOptions := Json
abbrev FoldingRangeRegistrationOptions := Json
abbrev ExecuteCommandOptions := Json
abbrev SelectionRangeRegistrationOptions := Json
abbrev LinkedEditingRangeRegistrationOption := Json
abbrev CallHierarchyRegistrationOptions := Json

example : ToJson (BOption EmptyObject) := inferInstance
example : ToJson (BOption SemanticTokensFullCapabilities) := inferInstance

structure SemanticTokensOptions extends WorkDoneProgressOptions where
  legend : SemanticTokensLegend
  range? : BOption EmptyObject := none
  full? : BOption SemanticTokensFullCapabilities := none
  deriving ToJson, FromJson

structure SemanticTokensRegistrationOptions extends
  SemanticTokensOptions, TextDocumentRegistrationOptions, StaticRegistrationOptions
  deriving ToJson, FromJson

abbrev MonikerRegistrationOptions := Json
abbrev TypeHierarchyRegistrationOptions := Json
abbrev InlineValueRegistrationOptions := Json
abbrev InlayHintRegistrationOptions := Json
abbrev DiagnosticRegistrationOptions := Json
abbrev WorkspaceSymbolOptions := Json
abbrev WorkspaceServerCapabilities := Json

structure ServerCapabilities (Experimental := Json) where
  positionEncoding? : Option PositionEncodingKind := none
  textDocumentSync? : Option TextDocumentSyncOptions := none
  notebookDocumentSync? : Option NotebookDocumentSyncRegistrationOptions := none
  completionProvider? : Option CompletionOptions := none
  hoverProvider? : BOption HoverOptions := none
  signatureHelpProvider? : Option SignatureHelpOptions := none
  declarationProvider? : BOption DeclarationRegistrationOptions := none
  definitionProvider? : BOption DefinitionOptions := none
  typeDefinitionProvider? : BOption TypeDefinitionRegistrationOptions := none
  implementationProvider? : BOption ImplementationRegistrationOptions := none
  referencesProvider? : BOption ReferenceOptions := none
  documentHighlightProvider? : BOption DocumentHighlightOptions := none
  documentSymbolProvider? : BOption DocumentSymbolOptions := none
  codeActionProvider? : BOption CodeActionOptions := none
  codeLensProvider? : Option CodeLensOptions := none
  documentLinkProver? : Option DocumentLinkOptions := none
  colorProvider? : BOption DocumentColorRegistrationOptions := none
  documentFormattingProvider? : BOption DocumentFormattingOptions := none
  documentRangeFormattingProvider? : BOption DocumentRangeFormattingOptions := none
  documentOnTypeFormattingProvider? : Option DocumentOnTypeFormattingOptions := none
  renameProvider? : BOption RenameOptions := none
  foldingRangeProvider? : BOption FoldingRangeRegistrationOptions := none
  executeCommandProvider? : Option ExecuteCommandOptions := none
  selectionRangeProvider? : BOption SelectionRangeRegistrationOptions := none
  linkedEditingRangeProvider? : BOption LinkedEditingRangeRegistrationOption := none
  callHierarchyProvider? : BOption CallHierarchyRegistrationOptions := none
  semanticTokensProvider? : Option SemanticTokensRegistrationOptions := none
  monikerProvider? : BOption MonikerRegistrationOptions := none
  typeHierarchyProvider? : BOption TypeHierarchyRegistrationOptions := none
  inlineValueProvider? : BOption InlineValueRegistrationOptions := none
  inlayHintProvider? : BOption InlayHintRegistrationOptions := none
  diagnosticProvider? : Option DiagnosticRegistrationOptions := none
  workspaceSymbolProvider? : BOption WorkspaceSymbolOptions := none
  workspace? : Option WorkspaceServerCapabilities := none
  experimental? : Option Experimental := none
  deriving Inhabited, ToJson, FromJson

instance [ToJson Exp] : Coe (ServerCapabilities Exp) ServerCapabilities where
  coe caps := {caps with experimental? := caps.experimental?.map toJson}
