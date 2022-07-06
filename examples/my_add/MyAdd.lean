import Alloy.C
open scoped Alloy.C

alloy c prelude
#include <lean/lean.h>
end

alloy c extern def myAdd (x y : UInt32) : UInt32 := {
  return x + y;
}
