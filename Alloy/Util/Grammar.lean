/-
Copyright (c) 2024 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Elab.ElabRules

namespace Alloy
open Lean Elab Command

syntax catWithPrio :=
  ident (":" prio)?

/--
Defines a piece of syntax with a antiquote kind and adds it to a category.
For example, `syntax foo : term := "foo"` is syntactic sugar for:

```
syntax foo := "foo"
attribute [term_parser] foo
```

For advanced use cases, a syntax can be added to multiple categories
and be assigned explicit priorities:

```
syntax foo : term:low, doElem:high := "foo"
```
-/
scoped syntax (name := syntaxCatAbbrev) (docComment)?
"syntax " ident " : " catWithPrio,* " := " stx* : command

elab_rules : command
| `($[$doc?]? syntax $id : $cats,* := $[$xs]*) => do
  let cmd ← `($[$doc?]? syntax $id := $[$xs]*)
  withMacroExpansion (← getRef) cmd <| elabCommand cmd
  let name ← resolveGlobalConstNoOverload id
  unless (← getEnv).contains name do
    return -- skip the attribute(s) if the declaration errored
  cats.getElems.forM fun cat => do
    let `(catWithPrio|$catId $[: $prio?]?) := cat
      | throwErrorAt cat "ill-formed category syntax:{indentD cat}"
    let catName := catId.getId
    unless (Parser.isParserCategory (← getEnv) catName) do
      throwErrorAt catId "unknown category '{catName}'"
    liftTermElabM <| Term.addCategoryInfo catId catName
    let attr := mkIdentFrom catId (catName.appendAfter "_parser")
    let cmd ← withRef cat `(attribute [$attr:ident $[$prio?:prio]?] $id)
    withMacroExpansion (← getRef) cmd <| elabCommand cmd
