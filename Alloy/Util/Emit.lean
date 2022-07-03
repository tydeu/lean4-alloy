/-! # Emitter Abstractions -/

open System
namespace Alloy

class MonadEmit (m : Type u → Type v) where
  emit : String → m PUnit

export MonadEmit (emit)

def emitLn [SeqRight m] [MonadEmit m] (str : String) : m PUnit :=
  emit str *> emit "\n"

abbrev EmitStrM := StateM String

instance : MonadEmit EmitStrM where
  emit str := modify (· ++ str)

def EmitStrM.run (self : EmitStrM PUnit) : String :=
  StateT.run self "" |>.run.2

abbrev EmitFileM := ReaderT IO.FS.Handle IO

instance : MonadEmit EmitFileM where
  emit str := fun h => h.putStr str

def EmitFileM.run (path : FilePath) (self : EmitFileM PUnit) : IO PUnit := do
  ReaderT.run self <| ← IO.FS.Handle.mk path .write true

abbrev EmitStreamM := ReaderT IO.FS.Stream IO

instance : MonadEmit EmitStreamM where
  emit str := fun h => h.putStr str

def EmitStreamM.run (stream : IO.FS.Stream) (self : EmitStreamM PUnit) : IO PUnit := do
  ReaderT.run self stream
