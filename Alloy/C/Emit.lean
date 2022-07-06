import Alloy.Util.Emit
import Alloy.C.Extension
import Lean.Compiler.IR.EmitC

open Lean
open IR (IRType)

namespace Alloy.C

/-! ## C Emitter -/

variable [Monad m] [MonadEmit m]

def emitType (type : IRType) : m PUnit :=
  emit (IR.EmitC.toCType type)

def emitParams (type : Expr) (ps : Array IR.Param) : m PUnit := do
  emit "("
  let mut fType := type
  -- Lean omits irrelevant parameters for extern constants
  let ps := ps.filter fun p => !p.ty.isIrrelevant
  for h : i in [0:ps.size] do
    if i > 0 then
      emit ", "
    let p := ps[⟨i, h.upper⟩]
    emitType p.ty
    emit " "
    if fType.isBinding then
      emit fType.bindingName!.toString
      fType := fType.bindingBody!
    else
      emit s!"_{i}"
  emit ")"

def emitDeclImpl (name : String) (decl : IR.Decl) (type : Expr) (impl : Syntax) : m PUnit := do
  emit "LEAN_EXPORT "
  emitType decl.resultType
  emit " "
  emit name
  emitParams type decl.params
  emit " "
  emitLn impl.reprint.get!.trim

def emitLocalShim (env : Environment) : m PUnit := do
  for cmd in C.cmdExt.getEntries env do
    emitLn cmd.reprint.get!.trim
  for (declName, impl) in C.implExt.getLocalEntries env do
    if let some info := env.find? declName then
    if let some decl := IR.findEnvDecl env declName then
    if let some name := getExternNameFor env `c decl.name then
      emitDeclImpl name decl info.type impl
