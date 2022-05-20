import Alloy.C.Extension
import Lean.Compiler.IR.EmitC

namespace Alloy.C

open Lean

abbrev EmitM := StateM String

def EmitM.toString (self : EmitM PUnit) : String :=
  self.run "" |>.run.2

def emit (str : String) : EmitM PUnit :=
  modify fun s => s ++ str

def emitLn (str : String) : EmitM PUnit := do
  emit str; emit "\n"

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
    if fType.isBinding then
      emit fType.bindingName!.toString
      fType := fType.bindingBody!
    else
      emit s!"_{i}"
  emit ")"

def emitDeclImpl (name : String) (decl : IR.Decl) (type : Expr) (impl : Syntax) : EmitM PUnit := do
  emit "LEAN_EXPORT "
  emitType decl.resultType
  emit " "
  emit name
  emitParams type decl.params
  emit " "
  emitLn impl.reprint.get!.trim

def emitLocalShim (env : Environment) : EmitM PUnit := do
  for cmd in C.cmdExt.getEntries env do
    emitLn cmd.reprint.get!.trim
  for (declName, impl) in C.implExt.getLocalEntries env do
    if let some info := env.find? declName then
    if let some decl := IR.findEnvDecl env declName then
    if let some name := getExternNameFor env `c decl.name then
      emitDeclImpl name decl info.type impl
