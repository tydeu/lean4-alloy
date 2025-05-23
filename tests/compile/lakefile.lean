import Lake
open System Lake DSL

package test

require alloy from ".."/".."

module_data alloy.c.o.export : FilePath
module_data alloy.c.o.noexport : FilePath
lean_lib Test where
  precompileModules := true
  nativeFacets := fun shouldExport =>
    if shouldExport then
      #[Module.oExportFacet, `module.alloy.c.o.export]
    else
      #[Module.oNoExportFacet, `module.alloy.c.o.noexport]


lean_lib Eval

@[default_target, test_driver]
lean_exe test where
  root := `Main
