import Lean.Parser

namespace Alloy

open Lean Parser PrettyPrinter

def raw (fn : ParserFn) (trailingWs := false) : Parser where
  fn := rawFn fn trailingWs

@[combinatorFormatter Alloy.raw] def raw.formatter :=
  Formatter.visitAtom Name.anonymous

@[combinatorParenthesizer Alloy.raw] def raw.parenthesizer :=
  Parenthesizer.visitToken

@[inline] def rawUntil (p : Char → Bool) (trailingWs := false) : Parser :=
  raw (takeUntilFn p) trailingWs

@[inline] def rawUntilCh (c : Char) (trailingWs := false) : Parser :=
  rawUntil (fun d => d == c) trailingWs

@[inline] def lineNoAntiquot : Parser :=
  rawUntilCh '\n'

@[runParserAttributeHooks, inline] def line : Parser :=
  withAntiquot (mkAntiquot "line" `line) lineNoAntiquot
