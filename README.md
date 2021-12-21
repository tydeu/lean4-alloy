# Alloy

Alloy is a Lean 4 library that allows one to embedded external FFI code (currently just C) within Lean definitions. For example, we can define an external C add function like so:

```lean
alloy c extern def my_add (x y : UInt32) : UInt32 := {
  return x + y;
}
```

And Alloy will generate the corresponding C function:

```c
LEAN_EXPORT uint32_t _impl_l_Foo_my__add(uint32_t x, uint32_t y) {
  return x + y;
}
```

See [S.lean](examples/S.lean) in the `examples` folder for a more complete example of Alloy's power.

Alloy is still a **work-in-progress**. Better Lake integration is needed before a proper stable version is released.
