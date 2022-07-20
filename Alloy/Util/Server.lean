/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Server.Requests
import Alloy.Util.Server.Worker

open Lean Server Lsp

namespace Alloy

def nullUri : DocumentUri :=
  if System.Platform.isWindows then
    "file:///nul"
  else
    "file:///dev/null"
