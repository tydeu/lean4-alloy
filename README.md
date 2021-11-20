# Alloy

Alloy is a Lean 4 library that allows one to embedded external FFI code (currently just C) within Lean definitions. For example, we can define an external C add function like so:

```lean
extern c def my_add (x y : UInt32) : UInt32 := {
  return x + y;
}
```

And Alloy will generate the corresponding C function:

```c
LEAN_EXPORT uint32_t my_add(uint32_t x, uint32_t y) {
  return x + y;
}
```

Alloy is still a **work-in-progress**. The full syntax of C is not yet supported and Lake integration is planned in the future.
