/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Syntax

open Lean
open IR (IRType)

namespace Alloy.C

def expandIrObjResultTypeToC : (borrow : Bool) → MacroM TypeSpec
| .true  => `(cTypeSpec|b_lean_obj_res)
| .false => `(cTypeSpec|lean_obj_res)

def expandIrResultTypeToC (borrow : Bool) : IRType → MacroM TypeSpec
| .float      => `(cTypeSpec|double)
| .uint8      => `(cTypeSpec|uint8_t)
| .uint16     => `(cTypeSpec|uint16_t)
| .uint32     => `(cTypeSpec|uint32_t)
| .uint64     => `(cTypeSpec|uint64_t)
| .usize      => `(cTypeSpec|size_t)
| .object     => expandIrObjResultTypeToC borrow
| .tobject    => expandIrObjResultTypeToC borrow
| .irrelevant => expandIrObjResultTypeToC borrow
| .struct ..  => Macro.throwError "unexpected IR type `struct`"
| .union ..   => Macro.throwError "unexpected IR type `union`"

def expandIrObjOParamTypeToC : (borrow : Bool) → MacroM TypeSpec
| .true  => `(cTypeSpec|b_lean_obj_arg)
| .false => `(cTypeSpec|lean_obj_arg)

def expandIrParamTypeToC (borrow : Bool) : IRType → MacroM TypeSpec
| .float      => `(cTypeSpec|double)
| .uint8      => `(cTypeSpec|uint8_t)
| .uint16     => `(cTypeSpec|uint16_t)
| .uint32     => `(cTypeSpec|uint32_t)
| .uint64     => `(cTypeSpec|uint64_t)
| .usize      => `(cTypeSpec|size_t)
| .object     => expandIrObjOParamTypeToC borrow
| .tobject    => expandIrObjOParamTypeToC borrow
| .irrelevant => expandIrObjOParamTypeToC borrow
| .struct ..  => Macro.throwError "unexpected IR type `struct`"
| .union ..   => Macro.throwError "unexpected IR type `union`"
