/-
Copyright (c) 2023 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Grammar
import Alloy.C.Command
import Lean.Elab.Eval

/-! # Lean ↔ C Translation
Definitions for wrapping and unwrapping C data in Lean types.
-/

open Lean Elab Syntax Command

namespace Alloy.C

/-- A translator stores the C functions used to wrap and unwrap a Lean FFI type. -/
structure Translator where
  /-- The name of the C function that wraps a C value in a Lean object. -/
  toLean : Name
  /-- The name of the C function that unwraps a C value from a Lean object. -/
  ofLean : Name
  deriving Inhabited

initialize translatorExt : MapDeclarationExtension Translator ←
  mkMapDeclarationExtension

/-! ## Registration -/

/--
The syntax `alloy c translator A := t`
evaluates `t` as a `Translator` and register its for the type named `A`.
-/
syntax "alloy " &"c " &"translator " ident " := "term : command

unsafe def evalTranslatorUnsafe (stx : Syntax) : TermElabM Translator :=
  Term.evalTerm Translator (mkConst ``Translator) stx

@[implemented_by evalTranslatorUnsafe]
opaque evalTranslator (stx : Syntax) : TermElabM Translator

elab_rules : command
| `(command| alloy c translator $typeId := $translator) => liftTermElabM do
  let declName ← resolveGlobalConstNoOverload typeId
  Term.addTermInfo' typeId (← mkConstWithLevelParams declName)
  let translator ← evalTranslator translator
  modifyEnv fun env => translatorExt.insert env declName translator

/-! ## C Usage -/

/-- Find the C translator associated with `type` or error. -/
def resolveTranslator (typeId : Ident) : TermElabM Translator := do
  let declName ← resolveGlobalConstNoOverload typeId
  Term.addTermInfo' typeId (← mkConstWithLevelParams declName)
  let some translator := translatorExt.find? (← getEnv) declName
    | throwErrorAt typeId s!"no translator defined for '{typeId.getId}'"
  return translator

/--
The C expression `to_lean<A>(a)` wraps
the C value `a` into the Lean type represented by `A`.
-/
scoped syntax:1000 "to_lean" "<" ident ">" "(" cExpr ")" : cExpr

elab_rules : shim
| `(cExpr| to_lean%$fTk<$leanType>(%$lp$val)%$rp) => MonadRef.withRef .missing do
  let translator ← liftTermElabM <| resolveTranslator leanType
  let exp ← `(cExpr|$(mkIdentFrom fTk translator.toLean):ident(%$lp$val)%$rp)
  elabShimSyntax exp

/--
The C expression `of_lean<A>(a)` unwraps the C value
stored in the Lean object `a` of the type `A`.
-/
scoped syntax:1000 "of_lean" "<" ident ">" "(" cExpr ")" : cExpr

elab_rules : shim
| `(cExpr| of_lean%$fTk<$leanType>(%$lp$val)%$rp) => MonadRef.withRef .missing do
  let translator ← liftTermElabM <| resolveTranslator leanType
  let exp ← `(cExpr|$(mkIdentFrom fTk translator.ofLean):ident(%$lp$val)%$rp)
  elabShimSyntax exp
