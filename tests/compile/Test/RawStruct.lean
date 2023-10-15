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

static lean_external_class * g_class_raw_Y = NULL;

static inline void Y_finalize(void* ptr) {
  free((Y*)ptr);
}

static inline void noop_foreach(void *mod, b_lean_obj_arg fn) {}

static inline lean_obj_res Y_to_Lean(Y* y) {
  if (g_class_raw_Y == NULL) {
    g_class_raw_Y = lean_register_external_class(Y_finalize, noop_foreach);
  }
  return lean_alloc_external(g_class_raw_Y, y);
}

static inline Y* Y_of_lean(lean_obj_arg y) {
  return (Y*)(lean_get_external_data(y));
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

alloy c translator RawY := {
  toLean := `Y_to_Lean
  ofLean := `Y_of_lean
}

alloy c section
LEAN_EXPORT lean_obj_res raw_Y_mk(lean_obj_arg y, lean_obj_arg u) {
  Y* rawY = malloc(sizeof(Y));
  rawY->n = lean_ctor_get_uint32(y, 0);
  rawY->m = lean_ctor_get_uint32(y, sizeof(uint32_t));
  return to_lean<RawY>(rawY);
}

LEAN_EXPORT uint32_t raw_Y_n(lean_obj_arg y) {
  return of_lean<RawY>(y)->n;
}
LEAN_EXPORT uint32_t raw_Y_m(lean_obj_arg y) {
  return of_lean<RawY>(y)->m;
}
end

attribute [extern "raw_Y_mk"] RawY.mk

@[extern "raw_Y_n"] def RawY.n (y : RawY) := y.data.n
@[extern "raw_Y_m"] def RawY.m (y : RawY) := y.data.m

def RawY.dataImpl (y : RawY) : PureY :=
  {n := y.n, m := y.m}

theorem RawY.dataImpl_eq_data (y : RawY) : y.data = y.dataImpl := rfl

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
  let r := addPureY {n := 4, m := 2}
  if r = 6 then
    IO.println "raw struct test passed"
  else
    throw <| IO.userError s!"raw struct test failed: {r} != 6"
