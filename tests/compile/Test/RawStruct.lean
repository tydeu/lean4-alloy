import Alloy.C
open scoped Alloy.C

-- TODO: Provide more Alloy helpers for this test.
-- Inspired by https://leanprover.zulipchat.com/#narrow/stream/270676-lean4/topic/lake.3A.20generating.20FFI.20bindings/near/396780175

namespace Test.RawStruct

alloy c include <lean/lean.h>

alloy c section
typedef struct {
  uint32_t n, m;
} Y;

static inline void Y_finalize(void* ptr) {
  free(ptr);
}
end

structure PureY where
  n : UInt32
  m : UInt32

structure RawY where
  data : PureY
  -- make the structure non-trivial
  -- this is unsafe hack around lean4#2292 for testing purposes
  unit : Unit

alloy c extern_type RawY => Y := {
  finalize := `Y_finalize
}

alloy c extern impl RawY.mk data := {
  Y* rawY = malloc(sizeof(Y));
  rawY->n = lean_ctor_get_uint32(data, 0);
  rawY->m = lean_ctor_get_uint32(data, sizeof(uint32_t));
  return to_lean<RawY>(rawY);
}

noncomputable def RawY.n (y : RawY) := y.data.n

alloy c extern impl RawY.n y :=
  return of_lean<RawY>(y)->n;

noncomputable def RawY.m (y : RawY) := y.data.m

alloy c extern impl RawY.m y :=
  return of_lean<RawY>(y)->m;

def RawY.dataImpl (y : RawY) : PureY :=
  {n := y.n, m := y.m}

theorem RawY.dataImpl_eq_data (y : RawY) : y.dataImpl = y.data := rfl

attribute [implemented_by RawY.dataImpl] RawY.data

set_option trace.compiler.ir.result true in
def addRawY (y : RawY) :=
  y.n + y.m

set_option trace.compiler.ir.result true in
def addPureY (y : PureY) :=
  addRawY <| .mk y () -- due to lean4#2292, will optimize away `mk` w/o Unit

def test' :=
  addPureY {n := 4, m := 2}

def test : IO Unit :=
  let r := test'
  if r = 6 then
    IO.println "raw struct test passed"
  else
    throw <| IO.userError s!"raw struct test failed: {r} != 6"
