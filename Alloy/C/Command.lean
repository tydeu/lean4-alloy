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
"alloy " &"c " &"section" ppLine cmds:cCmd+ ppLine "end" : command => do
  let env ← getEnv
  let shim := shimExt.getState env
  match shim.appendCmds? cmds with
  | .ok shim => setEnv <| shimExt.setState env shim
  | .error cmd => throwErrorAt cmd "command is ill-formed (cannot be reprinted)"

scoped macro (name := includeCmd)
"alloy " &"c " &"include " hdrs:header+ : command =>
  `(alloy c section $[#include $hdrs]* end)

def bracketedBinders :=
  many Term.bracketedBinder

scoped elab (name := externDecl) doc?:«docComment»?
"alloy " &"c " ex:&"extern " sym?:«str»? attrs?:Term.«attributes»?
"def " id:declId bs:bracketedBinders " : " type:term " := " body:cStmt : command => do

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
  let bs' := bs.raw.getArgs.map (⟨.⟩)
  let cmd ← `($[$doc?]? @[$attrs,*] opaque $id:declId $[$bs']* : $type)
  withMacroExpansion (← getRef) cmd <| elabCommand cmd

  -- C Definition
  let env ← getEnv
  if let some info := env.find? name then
    if let some decl := IR.findEnvDecl env name then
      let id := mkIdentFrom symLit (Name.mkSimple extSym)
      let (ty, ptr?) ← liftMacroM <| withRef type <| expandIrTypeToC decl.resultType
      let params ← liftMacroM <| withRef bs <| expandIrParamsToC info.type decl.params
      let (head, decls, stmts, tail) := unpackStmtBody body
      let body := packBody body
      let fn ← `(function|
        LEAN_EXPORT%$ex $ty:cTypeSpec $[$ptr?:pointer]?
        $id:ident(%$bs$params:params)%$bs $body:compStmt
      )
      let cmd ← `(alloy c section $fn:function end)
      withMacroExpansion (← getRef) cmd <| elabCommand cmd
      modifyEnv fun env => implExt.insert env name fn
    else
      throwError "failed to find Lean IR definition"
  else
    throwError "failed to find Lean definition"
