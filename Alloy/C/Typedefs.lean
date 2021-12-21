import Alloy.C.Syntax

/-!
Declarations of common C types used in Alloy code.
-/

namespace Alloy.C

syntax "uint8_t" : cTypeSpec
syntax "uint16_t" : cTypeSpec
syntax "uint32_t" : cTypeSpec
syntax "uint64_t" : cTypeSpec
syntax "size_t" : cTypeSpec

syntax "lean_object" : cTypeSpec
syntax "lean_obj_arg" : cTypeSpec
syntax "b_lean_obj_arg" : cTypeSpec
syntax "lean_obj_res" : cTypeSpec
syntax "b_lean_obj_res" : cTypeSpec
syntax "lean_external_class" : cTypeSpec
