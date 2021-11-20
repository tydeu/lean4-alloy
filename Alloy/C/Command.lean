import Alloy.C.Syntax
import Alloy.C.Extension

namespace Alloy.C

open Lean

open Parser Command

scoped syntax (name := externCInclude)
"extern " &"c " &"include " str : command

scoped syntax (name := externCDecl) «docComment»?
"extern " &"c " «str»? "def " declId declSig " := " cStmt : command

open Elab Command

@[commandElab externCInclude]
def elabExternCInclude : CommandElab := fun stx =>
  match stx with
  | `(extern c include $path) =>
    modifyEnv fun env => C.includeExt.addEntry env path.isStrLit?.get!
  | _ =>
    throwError "ill-formed external C include"

def toLowerName : Name → Name
| Name.str p s _ => Name.mkStr (toLowerName p) s.toLower
| n => n

@[commandElab externCDecl]
def elabExternCDecl : CommandElab := fun stx =>
  match stx with
  | `($[$doc?]? extern c $[$sym?]? def $id $sig := $body) => do
    if body.reprint.isNone then
      throwErrorAt body "body is ill-formed (cannot be printed)"
    let name := (← getCurrNamespace) ++ id[0].getId
    let defaultSym := name.toStringWithSep "_" false
    let symLit := sym?.getD <| Syntax.mkStrLit <| defaultSym
    let exp ← `($[$doc?]? @[extern $symLit:strLit] constant $id $sig)
    withMacroExpansion stx exp <| elabCommand exp
    modifyEnv fun env => C.implExt.insert env name body
  | _ =>
    throwError "ill-formed external C definition"
