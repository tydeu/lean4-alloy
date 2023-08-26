import Alloy.C

open Alloy C
open Lean Elab Command Syntax Parser

-- Test using a Lean command elaborator for a C command

elab_rules : command
| `(cCmd| #define $id $v) => do
  let some n := v.raw.reprint.bind (·.trim.toNat?)
    | throwErrorAt v "expected nat"
  addCommandToShim (← getRef)
  elabCommand <| ← `(def $id := $(quote n))

alloy c section
#define FOO 42
end

#eval show IO _ from do
  unless FOO == 42 do
    throw <| .userError s!"expected 42, got {FOO}"

-- Test using an Alloy elaborator for a piece of C syntax (and expression)

syntax (name := add) "add" "<" num "," num ">" : cExpr

elab_rules : shim
| `(cExpr| add<$x,$y>) => do
  elabShimSyntax <| ← `(cExpr| $(quote <| x.getNat + y.getNat))

alloy c section
static inline four = add<2,2>;
end

#eval show CommandElabM _ from do
  let shim := C.getLocalShim (← getEnv)
  IO.print shim.toString
  unless shim.cmds.size == 2 do
    throwError s!"expected 2 command, got {shim.cmds.size}"
  unless shim.toString.trim.endsWith "four = 4 ;" do
    throwError s!"shim missing expected end"
