import Alloy.C
open scoped Alloy.C

namespace Test.Enum

alloy c include <lean/lean.h>

alloy c enum
  MyNum => uint32_t
  | a => 1
  | b => 3
  | c => 7
  deriving Inhabited

alloy c extern def myAdd (x y : MyNum) : UInt32 :=
  return of_lean<MyNum>(x) + of_lean<MyNum>(y)

def test : IO Unit := do
  if myAdd .a .c = 8 then
    IO.println "enum test passed"
  else
    throw <| IO.userError "enum test failed"
