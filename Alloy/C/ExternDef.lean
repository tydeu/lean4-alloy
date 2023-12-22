/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.ExternImpl

namespace Alloy.C
open Lean Parser Elab Command

/--
Create an opaque Lean definition implemented by an external C function
whose definition is provided here. That is:

```
alloy c extern "alloy_foo" def foo (x : UInt32) : UInt32 := {...}
```

is essentially equivalent to

```
@[extern "alloy_foo"] opaque foo (x : UInt32) : UInt32
alloy c section LEAN_EXPORT uint32_t alloy_foo(uint32_t x) {...}
```
-/
scoped syntax (name := externDecl)
(docComment)? (Term.attributes)? (visibility)? «unsafe»?
"alloy " &"c " &"extern " (str)?
"def " declId binders " : " term " := " withPosition(many1Indent(cStmtLike))
: command

elab_rules : command
| `(externDecl| $[$doc?]? $[$attrs?]? $[$vis?]? $[unsafe%$uTk?]?
  alloy c extern%$exTk $[$sym?]? def $id $bs* : $ty := $stmts*) => do
  let cmd ← `($[$doc?]? $[$attrs?]? $[$vis?]? noncomputable $[unsafe%$uTk?]? opaque $id $[$bs]* : $ty)
  withMacroExpansion (← getRef) cmd <| elabCommand cmd
  let bvs ← liftMacroM <| bs.concatMapM matchBinder
  elabExternImpl exTk sym? ⟨id.raw[0]⟩ bvs ty (packBody stmts)
