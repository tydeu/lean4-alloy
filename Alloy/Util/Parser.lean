/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Parser

namespace Alloy

open Lean Parser PrettyPrinter Formatter Parenthesizer

def identSatisfyFn (expected : List String) (p : Name → Bool) : ParserFn := fun ctx st =>
  let startPos := st.pos
  let initStackSz := st.stackSize
  let st := tokenFn expected ctx st
  if st.hasError then
    st
  else
    match st.stxStack.back with
    | Syntax.ident _ _ val _ =>
      if p val then st else
        st.mkErrorsAt expected startPos initStackSz
    | _  => st.mkErrorsAt expected startPos initStackSz

def identSatisfyNoAntiquot (expected : List String) (p : Name → Bool)  : Parser where
  fn   := identSatisfyFn expected p
  info := mkAtomicInfo "ident"

@[combinator_formatter identSatisfyNoAntiquot]
def simpleIdentNoAntiquot.formatter := identNoAntiquot.formatter

@[combinator_parenthesizer identSatisfyNoAntiquot]
def simpleIdentNoAntiquot.parenthesizer := identNoAntiquot.parenthesizer

@[run_parser_attribute_hooks] def identSatisfy (expected : List String) (p : Name → Bool) : Parser :=
  withAntiquot (mkAntiquot "ident" identKind) <| identSatisfyNoAntiquot expected p

def raw (fn : ParserFn) (trailingWs := false) : Parser where
  fn := rawFn fn trailingWs

@[combinator_formatter raw] def raw.formatter := visitAtom Name.anonymous
@[combinator_parenthesizer raw] def raw.parenthesizer := visitToken

/-- Like `Lean.Parser.withAntiquot`, but with an `acceptLhs` option. -/
def withAntiquot (antiquotP p : Parser) (acceptLhs := false) : Parser where
  fn := withAntiquotFn antiquotP.fn p.fn acceptLhs
  info := orelseInfo antiquotP.info p.info

@[combinator_formatter withAntiquot]
def withAntiquot.formatter := Formatter.withAntiquot.formatter

@[combinator_parenthesizer withAntiquot]
def withAntiquot.parenthesizer := Parenthesizer.withAntiquot.parenthesizer

/-- Matches to the next unescaped linebreak. -/
partial def lineFn : ParserFn := fun c s =>
  parseNormal c.input s
where
  parseNormal input s :=
    if h : input.atEnd s.pos then
      s
    else
      let curr := input.get' s.pos h
      let s := s.next' input s.pos h
      if curr = '\\' then
        parseEscape input s
      else if curr = '\n' then
        s
      else
        parseNormal input s
  parseEscape input s :=
    if h : input.atEnd s.pos then
      s
    else
      let curr := input.get' s.pos h
      let s := s.next' input s.pos h
      if curr = '\\' then
        parseEscape input s
      else if curr = '\r' then
        if h : input.atEnd s.pos then
          s
        else
          let curr := input.get' s.pos h
          let s := s.next' input s.pos h
          if curr = '\\' then
            parseEscape input s
          else
            parseNormal input s
      else
        parseNormal input s

@[inherit_doc lineFn]
def lineNoAntiquot :=
  raw lineFn true

@[run_parser_attribute_hooks, inherit_doc lineFn]
def line : Parser :=
  withAntiquot (mkAntiquot "line" `line (isPseudoKind := true)) lineNoAntiquot true
