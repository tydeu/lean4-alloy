/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Data.Lsp.InitShutdown
import Alloy.Util.Server.Capabilities

open Lean Lsp

namespace Alloy

/-- Like `Lean.Lsp.Params` but more specification compliant. -/
structure InitializeParams (InitializationOptions ExperimentalCapabilities := Json) where
  processId? : Option Int := none
  clientInfo? : Option ClientInfo := none
  rootUri? : Option String := none
  initializationOptions? : Option InitializationOptions := none
  capabilities : ClientCapabilities ExperimentalCapabilities := {}
  trace? : Option Trace := none -- default: Trace.off
  workspaceFolders? : Option (Array WorkspaceFolder) := none
  deriving Inhabited

instance [ToJson Opts] [ToJson Exp] : Coe (InitializeParams Opts Exp) InitializeParams where
  coe p := {p with
    initializationOptions? := p.initializationOptions?.map toJson
    capabilities := p.capabilities
  }

/-- Instance from the Lean core adapted to the updated `InitializeParams` -/
instance [FromJson Opts] [FromJson Exp] : FromJson (InitializeParams Opts Exp) where
  fromJson? j := do
    let processId? := j.getObjValAs? Int "processId"
    let clientInfo? := j.getObjValAs? ClientInfo "clientInfo"
    let rootPath? := j.getObjValAs? String "rootPath"
    let rootUri? := j.getObjValAs? String "rootUri"
    let initializationOptions? := j.getObjValAs? Opts "initializationOptions"
    let capabilities ← j.getObjValAs? (ClientCapabilities Exp) "capabilities"
    let trace := (j.getObjValAs? Trace "trace").toOption.getD Trace.off
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

/-- We manually define this instance to make `rootUri?` null if missing. -/
instance [ToJson Opts] [ToJson Exp] : ToJson (InitializeParams Opts Exp) where
  toJson p := Id.run do
    let mut kvPairs := Std.RBNode.leaf
    if let some processId := p.processId? then
      kvPairs := kvPairs.insert compare "processId" <| toJson processId
    if let some clientInfo := p.clientInfo? then
      kvPairs := kvPairs.insert compare "clientInfo" <| toJson  clientInfo
    -- Root URI can be `null` but not `undefined` according to specification.
    kvPairs := kvPairs.insert compare "rootUri" <|
      match p.rootUri? with | some uri => toJson uri | none => Json.null
    if let some options := p.initializationOptions? then
      kvPairs := kvPairs.insert compare "initializationOptions" <| toJson  options
    kvPairs := kvPairs.insert compare "capabilities" <| toJson p.capabilities
    if let some trace := p.trace? then
      kvPairs := kvPairs.insert compare "trace" <| toJson trace
    if let some workspaceFolders := p.workspaceFolders? then
      kvPairs := kvPairs.insert compare "workspaceFolders" <| toJson workspaceFolders
    return Json.obj kvPairs
