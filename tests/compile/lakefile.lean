import Lake
open System Lake DSL

package my_add

require alloy from ".."/".."
module_data alloy.c.o : BuildJob FilePath

lean_lib Test where
  precompileModules := true
  nativeFacets := #[Module.oFacet, `alloy.c.o]

lean_lib Eval

@[default_target]
lean_exe run where
  root := `Main
