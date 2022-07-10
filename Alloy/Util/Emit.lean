/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/

/-! # Emitter Abstractions -/

open System
namespace Alloy

class MonadEmit (m : Type u → Type v) where
  emit : String → m PUnit

export MonadEmit (emit)

instance [MonadLift m n] [MonadEmit m] : MonadEmit n where
  emit s := liftM (m := m) <| emit s

@[inline] def emitLn [SeqRight m] [MonadEmit m] (str : String) : m PUnit :=
  emit str *> emit "\n"

instance : MonadEmit IO where
  emit := IO.print

abbrev EmitStrM := StateM String

instance : MonadEmit EmitStrM where
  emit str := modify (· ++ str)

@[inline] def EmitStrM.run (self : EmitStrM PUnit) : String :=
  StateT.run self "" |>.run.2

abbrev EmitFileM := ReaderT IO.FS.Handle IO

instance : MonadEmit EmitFileM where
  emit str := fun h => h.putStr str

@[inline] def EmitFileM.run (path : FilePath) (self : EmitFileM PUnit) : IO PUnit := do
  ReaderT.run self <| ← IO.FS.Handle.mk path .write true

abbrev EmitStreamM := ReaderT IO.FS.Stream IO

instance : MonadEmit EmitStreamM where
  emit str := fun h => h.putStr str

@[inline] def EmitStreamM.run (stream : IO.FS.Stream) (self : EmitStreamM PUnit) : IO PUnit := do
  ReaderT.run self stream
