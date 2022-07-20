/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Compiler.IR.Basic

open Lean
namespace Alloy

def irParamsToBindings
(fnType : Lean.Expr) (irParams : Array IR.Param)
: Array (IR.IRType × Name) := Id.run do
  let mut params := #[]
  let mut fnType := fnType
  -- Lean omits irrelevant parameters for extern constants
  let irParams := irParams.filter fun p => !p.ty.isIrrelevant
  for h : i in [0:irParams.size] do
    have : i < irParams.size := h.upper
    let p := irParams[i]
    if fnType.isBinding then
      params := params.push (p.ty, fnType.bindingName!)
      fnType := fnType.bindingBody!
    else
      params := params.push (p.ty, Name.mkSimple s!"_{i}")
  return params
