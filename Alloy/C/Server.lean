/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Server.Location
import Alloy.C.Server.SemanticTokens

open Lean Server Lsp

namespace Alloy.C

initialize
  chainLspRequestHandler "textDocument/hover" handleHover
  chainLspRequestHandler "textDocument/completion" handleCompletion
  chainStatelessLspRequestHandler "textDocument/semanticTokens/full" handleSemanticTokensFull
  chainLspRequestHandler "textDocument/semanticTokens/range" handleSemanticTokensRange
  chainLspRequestHandler "textDocument/declaration" (handleGoto "textDocument/declaration")
  chainLspRequestHandler "textDocument/definition" (handleGoto "textDocument/definition")
  chainLspRequestHandler "textDocument/typeDefinition" (handleGoto "textDocument/typeDefinition")
