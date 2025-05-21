import Lake
open System Lake DSL

package my_add

require alloy from ".."/".."

module_data alloy.c.o.export : FilePath
module_data alloy.c.o.noexport : FilePath
lean_lib MyAdd where
  precompileModules := true
  nativeFacets := fun shouldExport =>
    if shouldExport then
      #[Module.oExportFacet, `module.alloy.c.o.export]
    else
      #[Module.oNoExportFacet, `module.alloy.c.o.noexport]

lean_lib Test

@[default_target]
lean_exe my_add where
  root := `Main
