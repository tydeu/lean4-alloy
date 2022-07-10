/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.Util.Emit
import Alloy.C.Extension

open Lean
open IR (IRType)

namespace Alloy.C

/-! ## C Emitter -/

variable [Monad m] [MonadEmit m]

def emitLocalShim (env : Environment) : m PUnit := do
  for cmd in C.cmdExt.getEntries env do
    emitLn cmd.raw.reprint.get!.trim
