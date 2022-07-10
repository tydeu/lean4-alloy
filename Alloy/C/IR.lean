/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.Util.IR
import Alloy.C.Syntax

open Lean
open IR (IRType)

namespace Alloy.C

def expandIrTypeToC : IRType → MacroM (TypeSpec × Option Pointer)
| .float      => return (← `(cTypeSpec|double), none)
| .uint8      => return (← `(cTypeSpec|uint8_t), none)
| .uint16     => return (← `(cTypeSpec|uint16_t), none)
| .uint32     => return (← `(cTypeSpec|uint32_t), none)
| .uint64     => return (← `(cTypeSpec|uint64_t), none)
| .usize      => return (← `(cTypeSpec|size_t), none)
| .object     => return (← `(cTypeSpec|lean_object), ← `(pointer| *))
| .tobject    => return (← `(cTypeSpec|lean_object), ← `(pointer| *))
| .irrelevant => return (← `(cTypeSpec|lean_object), ← `(pointer| *))
| .struct ..  => Macro.throwError "unexpected IR type `struct`"
| .union ..   => Macro.throwError "unexpected IR type `union`"

def expandIrBindingsToCParamDecls
(bindings : Array (IRType × Name)) : MacroM (Array ParamDecl) :=
  bindings.mapM fun (ty, name) => do
    let id ← mkIdentFromRef name
    let (ty, ptr?) ← expandIrTypeToC ty
    `(paramDecl| $ty:cTypeSpec $[$ptr?:pointer]? $id:ident)

def expandIrParamsToC (fnType : Lean.Expr) (irParams : Array IR.Param) : MacroM Params := do
  let decls ← expandIrBindingsToCParamDecls (irParamsToBindings fnType irParams)
  `(params| $[$decls:paramDecl],*)
