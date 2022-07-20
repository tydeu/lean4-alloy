/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Server.Hover
import Alloy.C.Server.SemanticTokens

open Lean Server Lsp

namespace Alloy.C

initialize
  chainLspRequestHandler "textDocument/hover" HoverParams (Option Hover) handleHover
  chainLspRequestHandler "textDocument/semanticTokens/full"  SemanticTokensParams SemanticTokens handleSemanticTokensFull
  chainLspRequestHandler "textDocument/semanticTokens/range" SemanticTokensRangeParams SemanticTokens handleSemanticTokensRange
