import Alloy.C.Syntax
import Alloy.C.Extension
import Lean.Compiler.NameMangling
import Lean.Elab

namespace Alloy.C

open Lean

open Parser Command

scoped syntax (name := preludeDecl)
"alloy " &"c " &"prelude" ppLine cCmd+ ppLine "end" : command

scoped syntax (name := externDecl) «docComment»?
"alloy " &"c " &"extern " «str»? "def " declId declSig " := " cStmt : command

open Elab Command

@[commandElab preludeDecl]
def elabPreludeDecl : CommandElab := fun stx =>
  match stx with
  | `(alloy c prelude $[$cmds]* end) =>
    modifyEnv fun env => cmdExt.addEntries env cmds
  | _ =>
    throwError "ill-formed C prelude"

@[commandElab externDecl]
def elabExternDecl : CommandElab := fun stx =>
  match stx with
  | `($[$doc?]? alloy c extern $[$sym?]? def $id $sig := $body) => do
    if body.raw.reprint.isNone then
      throwErrorAt body "body is ill-formed (cannot be printed)"
    let name := (← getCurrNamespace) ++ id.raw[0].getId
    let symLit := sym?.getD <| Syntax.mkStrLit <| "_impl_" ++ name.mangle
    let exp ← `($[$doc?]? @[extern $symLit:str] opaque $id $sig)
    withMacroExpansion stx exp <| elabCommand exp
    modifyEnv fun env => implExt.insert env name body
  | _ =>
    throwError "ill-formed external C definition"
