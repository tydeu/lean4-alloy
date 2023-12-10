import Lake
open System Lake DSL

package my_add

require alloy from ".."/".."

module_data alloy.c.o : BuildJob FilePath
lean_lib MyAdd where
  precompileModules := true
  nativeFacets := #[Module.oFacet, `alloy.c.o]

lean_lib Test

@[default_target]
lean_exe my_add where
  root := `Main
