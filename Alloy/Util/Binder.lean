
/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Parser.Term

open Lean Parser Syntax

namespace Alloy

-- Adapted from the private utilities in `Lean.Elab.Binders`

abbrev Hole := TSyntax ``Term.hole
abbrev BinderIdent := TSyntax ``Term.binderIdent
abbrev BinderModifier := TSyntax [``Term.binderTactic, ``Term.binderDefault]

def mkHoleFrom (ref : Syntax) : Hole :=
  mkNode ``Term.hole #[mkAtomFrom ref "_"]

instance : Coe Hole Term where
  coe s := ⟨s.raw⟩

  instance : Coe Hole BinderIdent where
  coe s := ⟨s.raw⟩

instance : Coe Ident BinderIdent where
  coe s := ⟨s.raw⟩

@[run_parser_attribute_hooks]
def binder := Term.binderIdent <|> Term.bracketedBinder

abbrev Binder := TSyntax ``binder

instance : Coe Binder (TSyntax [identKind, ``Term.hole, ``Term.bracketedBinder]) where
  coe stx := ⟨stx.raw⟩

@[run_parser_attribute_hooks]
def binders :=  many (ppSpace >> binder)

structure BinderSyntaxView where
  ref : Syntax
  id : BinderIdent
  type : Term
  info : BinderInfo
  modifier? : Option BinderModifier := none

def expandOptType (ref : Syntax) (optType : Syntax) : Term :=
  if optType.isNone then
    mkHoleFrom ref
  else
    ⟨optType[0][1]⟩

def getBinderIds (ids : Syntax) : MacroM (Array BinderIdent) :=
  ids.getArgs.mapM fun id =>
    let k := id.getKind
    if k == identKind || k == `Lean.Parser.Term.hole then
      return ⟨id⟩
    else
      Macro.throwErrorAt id "identifier or `_` expected"

def expandOptIdent (stx : Syntax) : BinderIdent :=
  if stx.isNone then mkHoleFrom stx else ⟨stx[0]⟩

def expandBinderType (ref : Syntax) (stx : Syntax) : Term :=
  if stx.getNumArgs == 0 then mkHoleFrom ref else ⟨stx[1]⟩

def expandBinderModifier (optBinderModifier : Syntax) : Option BinderModifier :=
  if optBinderModifier.isNone then
    none
  else
    some ⟨optBinderModifier[0]⟩

def matchBinder (stx : Syntax) : MacroM (Array BinderSyntaxView) := do
  let k := stx.getKind
  if stx.isIdent || k == ``Term.hole then
    -- binderIdent
    return #[{
      ref := stx
      id := ⟨stx⟩
      type := mkHoleFrom stx
      info := .default
    }]
  else if k == ``Lean.Parser.Term.explicitBinder then
    -- `(` binderIdent+ binderType (binderDefault <|> binderTactic)? `)`
    let ids ← getBinderIds stx[1]
    let type := stx[2]
    let modifier? := expandBinderModifier stx[3]
    return ids.map fun id =>  {
      ref := stx
      id
      type := expandBinderType id type
      info := .default
      modifier?
    }
  else if k == ``Lean.Parser.Term.implicitBinder then
    -- `{` binderIdent+ binderType `}`
    let ids ← getBinderIds stx[1]
    let type := stx[2]
    return ids.map fun id => {
      ref := stx
      id
      type := expandBinderType id type
      info := .implicit
    }
  else if k == ``Lean.Parser.Term.strictImplicitBinder then
    -- `⦃` binderIdent+ binderType `⦄`
    let ids ← getBinderIds stx[1]
    let type := stx[2]
    return ids.map fun id => {
      ref := stx
      id
      type := expandBinderType id type
      info := .strictImplicit
    }
  else if k == ``Lean.Parser.Term.instBinder then
    -- `[` optIdent type `]`
    return #[{
      ref := stx
      id := expandOptIdent stx[1]
      type := ⟨stx[2]⟩
      info := .instImplicit
    }]
  else
    Macro.throwUnsupported
