import Lake
open System Lake DSL

package my_add

require alloy from ".."/".."

module_data alloy.c.o : ActiveFileTarget
lean_lib MyAdd {
  precompileModules := true
  nativeFacets := #[Module.oFacet, `alloy.c.o]
}

lean_lib Test

@[defaultTarget]
lean_exe my_add {
  root := `Main
}
