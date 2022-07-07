import Alloy.C
open scoped Alloy.C

/-!
An adaption of Lean 4's
['foreign'](https://github.com/leanprover/lean4/tree/b278a20ac22adcbfde11db386f2dc874d4a215ad/tests/compiler/foreign)
example for Alloy.
-/

--------------------------------------------------------------------------------
-- S Type
--------------------------------------------------------------------------------

alloy c prelude

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <lean/lean.h>

typedef struct {
  uint32_t      m_x;
  uint32_t      m_y;
  lean_object * m_s;
} S;

end

opaque S.nonemptyType : NonemptyType
def S : Type := S.nonemptyType.type
instance : Nonempty S := S.nonemptyType.property

--------------------------------------------------------------------------------
-- C Utils for S
--------------------------------------------------------------------------------

alloy c prelude

static void S_finalize(void* ptr) {
  lean_dec(((S*)ptr)->m_s);
  free(ptr);
}

static void S_foreach(void* ptr, b_lean_obj_arg f) {
  lean_apply_1(f, ((S*)ptr)->m_s);
}

static lean_external_class * g_S_class = NULL;

static inline lean_object * S_to_lean(S* s) {
  if (g_S_class == NULL) {
    g_S_class = lean_register_external_class(S_finalize, S_foreach);
  }
  return lean_alloc_external(g_S_class, s);
}

static inline S const * to_S(b_lean_obj_arg s) {
  return (S*)(lean_get_external_data(s));
}

static S g_s = {0, 0, NULL};

end

--------------------------------------------------------------------------------
-- Lean Bridge
--------------------------------------------------------------------------------

alloy c extern "lean_mk_S"
def mkS (x y : UInt32) (string : String) : S := {
  S* s = malloc(sizeof(S));
  s->m_x = x;
  s->m_y = y;
  s->m_s = string;
  return S_to_lean(s);
}

alloy c extern "lean_S_add_x_y"
def S.addXY (s : @& S) : UInt32 := {
  return to_S(s)->m_x + to_S(s)->m_y;
}

alloy c extern "lean_S_string"
def S.string (s : @& S) : String := {
  lean_inc(to_S(s)->m_s);
  return to_S(s)->m_s;
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

alloy c extern "lean_S_update_global"
def updateGlobalS (s : @& S) : BaseIO Unit := {
  if (g_s.m_s != NULL) {
    lean_dec(g_s.m_s);
  }
  lean_inc(to_S(s)->m_s);
  g_s.m_x = to_S(s)->m_x;
  g_s.m_y = to_S(s)->m_y;
  g_s.m_s = to_S(s)->m_s;
  return lean_io_result_mk_ok(lean_box(0));
}
