/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Parser

namespace Alloy

open Lean Parser PrettyPrinter

def raw (fn : ParserFn) (trailingWs := false) : Parser where
  fn := rawFn fn trailingWs

@[combinator_formatter Alloy.raw] def raw.formatter :=
  Formatter.visitAtom Name.anonymous

@[combinator_parenthesizer Alloy.raw] def raw.parenthesizer :=
  Parenthesizer.visitToken

@[inline] def rawUntil (p : Char → Bool) (trailingWs := false) : Parser :=
  raw (takeUntilFn p) trailingWs

@[inline] def rawUntilCh (c : Char) (trailingWs := false) : Parser :=
  rawUntil (fun d => d == c) trailingWs

@[inline] def lineNoAntiquot : Parser :=
  rawUntilCh '\n' true

/-- Like `Lean.Parser.withAntiquot`, but with an `acceptLhs` option. -/
def withAntiquot (antiquotP p : Parser) (acceptLhs := false) : Parser := {
  fn := withAntiquotFn antiquotP.fn p.fn acceptLhs
  info := orelseInfo antiquotP.info p.info
}

@[combinator_formatter withAntiquot]
def withAntiquot.formatter :=
  Formatter.withAntiquot.formatter

@[combinator_parenthesizer withAntiquot]
def withAntiquot.parenthesizer :=
  Parenthesizer.withAntiquot.parenthesizer

@[run_parser_attribute_hooks, inline] def line : Parser :=
  withAntiquot (mkAntiquot "line" `line (isPseudoKind := true)) lineNoAntiquot true
