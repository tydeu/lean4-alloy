import Alloy.C.Grammar
import Lean.Elab.Command

open Lean Parser Elab Command

partial def Lean.Data.Trie.erase {α} (t : Trie α) (s : String) : Trie α :=
  let rec loop : Nat → Trie α → Trie α
    | i, leaf v =>
      if i < s.utf8ByteSize then leaf v else leaf none
    | i, node1 v c' t' =>
      if h : i < s.utf8ByteSize then
        if c' = s.getUtf8Byte i h then
          loop (i+1) t'
        else
          node1 v c' t'
      else
        node1 none c' t'
    | i, node v cs ts =>
      if h : i < s.utf8ByteSize then
        let c := s.getUtf8Byte i h
        match cs.findIdx? (· == c) with
        | none => node v cs ts
        | some idx => node v cs <| ts.setD idx <| loop (i+1) (ts.get! idx)
      else
        node none cs ts
  loop 0 t

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
