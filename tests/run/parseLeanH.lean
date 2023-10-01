import Alloy.C.Grammar
import Lean.Elab.Command

open Lean Parser Elab Command

/- By  Mario: https://leanprover.zulipchat.com/#narrow/stream/270676-lean4/topic/Undefine.20lambda/near/393694214-/
def Lean.Parser.Trie.isEmpty {α} (t : Trie α) : Bool := t matches .Node none .leaf
partial def Lean.Parser.Trie.erase {α} (t : Trie α) (s : String) : Trie α :=
  let rec loop : Trie α → String.Pos → Option (Trie α)
    | ⟨val, m⟩, i =>
      match s.atEnd i with
      | true  => some (Trie.Node none m)
      | false => do
        let c := s.get i
        let i := s.next i
        let t ← m.find compare c
        let t ← loop t i
        let m := if t.isEmpty then m.erase compare c else m.insert compare c t
        some ⟨val, m⟩
  (loop t 0).getD t

syntax "LEAN_CASSERT" "(" cExpr ")" ";" : cCmd
syntax "extern" str "{" : cCmd
syntax "}" : cCmd

partial def test : CommandElabM PUnit := do
  let fileName := "lean.h"
  let includeDir := (← Lean.getBuildDir) / "include" / "lean"
  let input ← IO.FS.readFile <| includeDir / fileName
  let input := input.replace "--" "-=1"
  let p := categoryParser `cCmd 0
  let ictx := mkInputContext input fileName
  let env ← getEnv
  let env := parserExtension.modifyState env fun s =>
    {s with tokens := s.tokens.insert "~" "~" |>.erase "fun" |>.erase "end"}
  let scope ← getScope
  let pmctx := {
    env, options := scope.opts,
    currNamespace := scope.currNamespace, openDecls := scope.openDecls
  }
  go p.fn ictx pmctx (mkParserState ictx.input)
where
  go p ictx pmctx s := do
    let s := p.run ictx pmctx (getTokenTable pmctx.env) s
    if s.hasError then
      logError <| s.toErrorMsg ictx
    else if ictx.input.atEnd s.pos then
      pure ()
    else
      go p ictx pmctx s

#eval test -- should not error
