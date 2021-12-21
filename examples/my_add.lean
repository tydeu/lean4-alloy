import Alloy.C

open scoped Alloy.C

namespace Foo

alloy c extern def my_add (x y : UInt32) : UInt32 := {
  return x + y;
}
