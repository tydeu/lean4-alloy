/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.IR
import Alloy.C.Extension
import Alloy.Util.Syntax
import Lean.Compiler.NameMangling
import Lean.Elab.ElabRules

namespace Alloy.C
open Lean Parser Elab Command

syntax (name := leanExport) "LEAN_EXPORT" : cDeclSpec

scoped elab (name := sectionCmd)
"alloy " &"c " &"section" ppLine cmds:cCmd+ ppLine "end" : command =>
  modifyEnv (cmdExt.modifyState · (·.append cmds))

scoped macro (name := includeCmd)
"alloy " &"c " &"include " hdrs:header+ : command =>
  `(alloy c section $[#include $hdrs]* end)

scoped elab (name := externDecl) doc?:«docComment»?
"alloy " &"c " &"extern " sym?:«str»? attrs?:Term.«attributes»?
"def " id:declId sig:declSig " := " body:cStmt : command => do
  if body.raw.reprint.isNone then
    throwErrorAt body "body is ill-formed (cannot be printed)"

  -- Lean Definition
  let name := (← getCurrNamespace) ++ id.raw[0].getId
  let (symLit, extSym) :=
    match sym? with
    | some sym => (sym, sym.getString)
    | none =>
      let extSym := "_impl_" ++ name.mangle
      (Syntax.mkStrLit extSym, extSym)
  let attr ← `(Term.attrInstance| extern $symLit:str)
  let attrs := #[attr] ++ expandAttrs attrs?
  let cmd ← `($[$doc?]? @[$attrs,*] opaque $id $sig)
  withMacroExpansion (← getRef) cmd <| elabCommand cmd

  -- C Definition
  let env ← getEnv
  if let some info := env.find? name then
    if let some decl := IR.findEnvDecl env name then
      let id := mkIdentFrom symLit (Name.mkSimple extSym)
      let (ty, ptr?) ← liftMacroM <| withRef sig <| expandIrTypeToC decl.resultType
      let params ← liftMacroM <| withRef sig <| expandIrParamsToC info.type decl.params
      let (head, decls, stmts, tail) := unpackStmtBody body
      let fn ← `(function|
        LEAN_EXPORT $ty:cTypeSpec $[$ptr?:pointer]? $id:ident($params:params) {%$head
          $decls*
          $[$stmts:cStmt]*
        }%$tail
      )
      let cmd ← `(alloy c section $fn:function end)
      withMacroExpansion (← getRef) cmd <| elabCommand cmd
      modifyEnv fun env => implExt.insert env name fn
    else
      throwError "failed to find Lean IR definition"
  else
    throwError "failed to find Lean definition"
