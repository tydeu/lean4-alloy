# Alloy

Alloy is a Lean 4 library that allows one to embed external FFI code (currently just C) directly within Lean. For example, we can define an external C add function like so:

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

## Building Shims

Alloy exploits Lake's experimental (as of [v3.2.0](https://github.com/leanprover/lake/releases/tag/v3.2.0)) module facets feature to automagically build the shim it produces when compiling the module. Combined with the new `precompileModules` feature, this allows the shim code to be directly used by importers in a interpreted context (e.g., for `#eval` or when editing).

To use Alloy with your project and build shims for a library, add the following to your Lakefile:

```lean
require alloy from git "https://github.com/tydeu/lean4-alloy.git"

module_data alloy.c.o : ActiveFileTarget
lean_lib <your-lib> {
  precompileModules := true
  nativeFacets := #[Module.oFacet, &`alloy.c.o]
  -- and whatever other configuration options you wish to add
}
```

Take a look at the [examples](examples) to see how all of this works. The [`my_add`](examples/my_add) example provides a minimal setup whereas the [`S`](examples/S) example provides a more complete demonstration of Alloy's power.

## Disclaimer

Alloy is still a **work-in-progress**. However, it is now at the point where it can be feasible used to build FFIs. Its biggest TODOs are LSP support for the embedded C code and adding more utilities to help with common code patterns (e.g., defining wrapped C structures).
