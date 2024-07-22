import Alloy.C
open scoped Alloy.C

namespace ScalarStruct

/-
Adapted from
https://leanprover.zulipchat.com/#narrow/stream/270676-lean4/topic/Alloy.3A.20using.20extern_type.20.28not.20opaque.29/near/453162160
-/

alloy c include <lean/lean.h>

alloy c section
struct winsize {
  uint16_t ws_row;
  uint16_t ws_col;
  uint16_t ws_xpixel;
  uint16_t ws_ypixel;
}
end

namespace Terminal

structure WinSize where
  row : UInt16
  col : UInt16
  xPixel : UInt16
  yPixel : UInt16
  deriving DecidableEq, Repr

alloy c extern def getWinSize : IO WinSize :=
  lean_object *o = lean_alloc_ctor(0, 0, sizeof(struct winsize))
  struct winsize *w = (struct winsize *)lean_ctor_obj_cptr(o)
  w->ws_row = 100
  w->ws_col = 80
  w->ws_xpixel = 0
  w->ws_ypixel = 0
  return lean_io_result_mk_ok(o)

end Terminal

def test : IO Unit := do
  let ws : Terminal.WinSize ← Terminal.getWinSize
  if ws == ⟨100, 80, 0, 0⟩ then
    IO.println "scalar struct test passed"
  else
    throw <| IO.userError s!"scalar struct test failed: mismatched WinSize: {repr ws}"
