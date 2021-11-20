import Alloy.C.Extension
import Lean.Compiler.IR.EmitC

namespace Alloy.C

open Lean

abbrev EmitM := StateM String

def EmitM.toString (self : EmitM PUnit) : String :=
  self.run "" |>.run.2

def emit (str : String) : EmitM PUnit :=
  modify fun s => s ++ str

def emitInclude (path : String) : EmitM PUnit :=
  emit s!"#include \"{path}\"\n"

def emitType (type : IR.IRType) : EmitM PUnit :=
  emit (IR.EmitC.toCType type)

def emitParams (type : Expr) (ps : Array IR.Param)  : EmitM PUnit := do
  emit "("
  let mut fType := type
  -- Lean omits irrelevant parameters for extern constants
  let ps := ps.filter fun p => !p.ty.isIrrelevant
  for i in [0:ps.size] do
    if i > 0 then
      emit ", "
    let p := ps[i]
    emitType p.ty
    emit " "
    emit fType.bindingName!.toString
    fType := fType.bindingBody!
  emit ")"

def emitDeclImpl (name : String) (decl : IR.Decl) (type : Expr) (impl : Syntax) : EmitM PUnit := do
  emit "LEAN_EXPORT "
  emitType decl.resultType
  emit " "
  emit name
  emitParams type decl.params
  emit " "
  emit impl.reprint.get!

def emitLocalShim (env : Environment) : EmitM PUnit := do
  emitInclude "lean/lean.h"
  for include in C.includeExt.getEntries env do
    emitInclude include
  for (declName, impl) in C.implExt.getLocalEntries env do
    if let some info := env.find? declName then
    if let some decl := IR.findEnvDecl env declName then
    if let some name := getExternNameFor env `c decl.name then
      emit "\n"; emitDeclImpl name decl info.type impl
