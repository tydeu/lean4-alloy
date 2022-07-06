import Lake
open Lake DSL

package s {
  buildType := .debug
}

require alloy from ".."/".."

module_data alloy.c.o : ActiveFileTarget
lean_lib S {
  precompileModules := true
  nativeFacets := #[Module.oFacet, &`alloy.c.o]
}

@[defaultTarget]
lean_exe s {
  root := `Main
}

lean_lib Test
