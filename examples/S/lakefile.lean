import Lake
open Lake DSL

package s {
  buildType := .debug
}

require alloy from ".."/".."

module_data alloy.c.o : BuildJob FilePath
lean_lib S {
  precompileModules := true
  nativeFacets := #[Module.oFacet, `alloy.c.o]
}

@[default_target]
lean_exe s {
  root := `Main
}

lean_lib Test
