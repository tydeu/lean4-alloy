/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Syntax
import Alloy.C.Extension
import Alloy.Util.Syntax
import Lean.Compiler.NameMangling
import Lean.Elab.ElabRules

namespace Alloy.C

open Lean

open Parser Command

scoped elab (name := preludeCmd)
"alloy " &"c " &"prelude" ppLine cmds:cCmd+ ppLine "end" : command =>
  modifyEnv fun env => cmdExt.addEntries env cmds

scoped macro (name := includeCmd)
"alloy " &"c " &"include " hdrs:header+ : command =>
  `(alloy c prelude $[#include $hdrs]* end)

open Elab Command

scoped elab (name := externDecl) doc?:«docComment»?
"alloy " &"c " &"extern " sym?:«str»? attrs?:Term.«attributes»?
"def " id:declId sig:declSig " := " body:cStmt : command => do
  if body.raw.reprint.isNone then
    throwErrorAt body "body is ill-formed (cannot be printed)"
  let name := (← getCurrNamespace) ++ id.raw[0].getId
  let symLit := sym?.getD <| Syntax.mkStrLit <| "_impl_" ++ name.mangle
  let attr ← `(Term.attrInstance| extern $symLit:str)
  let attrs := #[attr] ++ expandAttrs attrs?
  let cmd ← `($[$doc?]? @[$attrs,*] opaque $id $sig)
  withMacroExpansion (← getRef) cmd <| elabCommand cmd
  modifyEnv fun env => implExt.insert env name body
