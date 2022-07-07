/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Parser.Term

namespace Alloy
open Lean Parser Term

def expandAttrs (attrs? : Option (TSyntax ``attributes)) : Array (TSyntax ``attrInstance) :=
  if let some attrs := attrs? then
    match attrs with
    | `(attributes| @[$attrs,*]) => attrs
    | _ => #[]
  else
    #[]
