import Alloy.C
open Alloy C

open Lean Elab Command

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

#eval show CommandElabM _ from do
  let shim := C.getLocalShim (← getEnv)
  IO.print shim.toString
  unless shim.cmds.size == 1 do
    throwError s!"expected 1 command, got {shim.cmds.size}"
