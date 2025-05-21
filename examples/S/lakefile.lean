import Lake
open System Lake DSL

package s where
  buildType := .debug

require alloy from ".."/".."

module_data alloy.c.o.export : FilePath
module_data alloy.c.o.noexport : FilePath
lean_lib S where
  precompileModules := true
  nativeFacets := fun shouldExport =>
    if shouldExport then
      #[Module.oExportFacet, `module.alloy.c.o.export]
    else
      #[Module.oNoExportFacet, `module.alloy.c.o.noexport]

@[default_target]
lean_exe s where
  root := `Main

lean_lib Test
