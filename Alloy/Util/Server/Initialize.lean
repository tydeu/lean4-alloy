/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Data.Lsp.InitShutdown
import Alloy.Util.Server.Capabilities

open Lean Lsp

namespace Alloy

/-- Like `Lean.Lsp.InitializeParams` but more specification compliant. -/
structure InitializeParams (InitializationOptions ExperimentalCapabilities := Json) where
  processId? : Option Int := none
  clientInfo? : Option ClientInfo := none
  rootUri : Option String := none
  initializationOptions? : Option InitializationOptions := none
  capabilities : ClientCapabilities ExperimentalCapabilities := {}
  trace? : Option Trace := none -- default: Trace.off
  workspaceFolders? : Option (Array WorkspaceFolder) := none
  deriving Inhabited, ToJson

instance [ToJson Opts] [ToJson Exp] : Coe (InitializeParams Opts Exp) InitializeParams where
  coe p := {p with
    initializationOptions? := p.initializationOptions?.map toJson
    capabilities := p.capabilities
  }

/--
Instance from the Lean core adapted to the updated `InitializeParams`
(also supports `rootPath`)
-/
instance [FromJson Opts] [FromJson Exp] : FromJson (InitializeParams Opts Exp) where
  fromJson? j := do
    let processId? := j.getObjValAs? Int "processId"
    let clientInfo? := j.getObjValAs? ClientInfo "clientInfo"
    let rootPath? := j.getObjValAs? String "rootPath"
    let rootUri? := j.getObjValAs? String "rootUri"
    let initializationOptions? := j.getObjValAs? Opts "initializationOptions"
    let capabilities ← j.getObjValAs? (ClientCapabilities Exp) "capabilities"
    let trace := (j.getObjValAs? Trace "trace").toOption
    let workspaceFolders? := j.getObjValAs? (Array WorkspaceFolder) "workspaceFolders"
    return ⟨
      processId?.toOption,
      clientInfo?.toOption,
      rootUri? <|> rootPath?.map (s!"file://{·}") |>.toOption,
      initializationOptions?.toOption,
      capabilities,
      trace,
      workspaceFolders?.toOption
    ⟩

/-- Like `Lean.Lsp.InitializeResult` but more specification compliant. -/
structure InitializeResult (ExperimentalCapabilities := Json) where
  capabilities : ServerCapabilities ExperimentalCapabilities := {}
  serverInfo? : Option ServerInfo := none
  deriving Inhabited, ToJson, FromJson

instance [ToJson Exp] : Coe (InitializeResult  Exp) InitializeResult where
  coe r := {r with capabilities := r.capabilities}
