import Alloy.C
open scoped Alloy.C

/-!
# `S.lean`

An adaption of Lean 4's ['foreign'][1] example for Alloy.

[1]: https://github.com/leanprover/lean4/tree/b278a20ac22adcbfde11db386f2dc874d4a215ad/tests/compiler/foreign
-/

alloy c include <stdint.h> <stdlib.h> <string.h> <lean/lean.h>

--------------------------------------------------------------------------------
/-! ## Definition of S                                                      -/
--------------------------------------------------------------------------------

alloy c section

typedef struct {
  uint32_t      m_x;
  uint32_t      m_y;
  lean_object * m_s;
} S;

static void S_finalize(void* ptr) {
  lean_dec(((S*)ptr)->m_s);
  free(ptr);
}

static void S_foreach(void* ptr, b_lean_obj_arg f) {
  lean_apply_1(f, ((S*)ptr)->m_s);
}

static S g_s = {0, 0, NULL};

end

alloy c extern_type S => S := {
  foreach := `S_foreach
  finalize := `S_finalize
}

--------------------------------------------------------------------------------
/-! ## Lean Interface                                                         -/
--------------------------------------------------------------------------------

alloy c extern "lean_mk_S"
def mkS (x y : UInt32) (string : String) : S := {
  S* s = malloc(sizeof(S));
  s->m_x = x;
  s->m_y = y;
  s->m_s = string;
  return to_lean<S>(s);
}

alloy c extern "lean_S_add_x_y"
def S.addXY (s : @& S) : UInt32 := {
  return of_lean<S>(s)->m_x + of_lean<S>(s)->m_y;
}

alloy c extern "lean_S_string"
def S.string (s : @& S) : String := {
  lean_inc(of_lean<S>(s)->m_s);
  return of_lean<S>(s)->m_s;
}

alloy c extern "lean_S_global_append"
def appendToGlobalS (string : String) : BaseIO PUnit := {
  if (g_s.m_s == NULL) {
    g_s.m_s = string;
  } else {
    g_s.m_s = lean_string_append(g_s.m_s, string);
  }
  return lean_io_result_mk_ok(lean_box(0));
}

alloy c extern "lean_S_global_string"
def getGlobalString : BaseIO String := {
  if (g_s.m_s == NULL) {
    g_s.m_s = lean_mk_string("");
  }
  lean_inc(g_s.m_s);
  return lean_io_result_mk_ok(g_s.m_s);
}

set_option trace.Elab.info true
alloy c extern "lean_S_update_global"
def updateGlobalS (s : @& S) : BaseIO Unit := {
  if (g_s.m_s != NULL) {
    lean_dec(g_s.m_s);
  }
  lean_inc(of_lean<S>(s)->m_s);
  g_s.m_x = of_lean<S>(s)->m_x;
  g_s.m_y = of_lean<S>(s)->m_y;
  g_s.m_s = of_lean<S>(s)->m_s;
  return lean_io_result_mk_ok(lean_box(0));
}
