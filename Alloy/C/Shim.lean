/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Syntax

open Lean

namespace Alloy.C

/-- A single command in a C Shim. -/
structure ShimCmd where
  /-- The Lean syntax of the command. -/
  stx : Cmd
  /-- Start of the command in the **C** source. -/
  startPos : String.Pos
  /-- End of the command in the **C** source. -/
  endPose : String.Pos
  deriving Inhabited

/--
Find the C source position within the command
corresponding to the `leanPos` in the Lean source.
-/
def ShimCmd.findPos? (self : ShimCmd) (leanPos : String.Pos) : Option String.Pos := do
  let some headPos := self.stx.raw.getPos? | failure
  let some tailPos := self.stx.raw.getTailPos? | failure
  if headPos < leanPos && leanPos < tailPos then
    return leanPos - headPos + self.startPos
  else
    failure

/-- A C shim -- an array of commands with shim source position information. -/
structure Shim where
  cmds : Array ShimCmd
  text : FileMap
  deriving Inhabited

namespace Shim

def empty : Shim :=
  ⟨{}, FileMap.ofString ""⟩

instance : EmptyCollection Shim := ⟨Shim.empty⟩

def isEmpty (self : Shim) : Bool :=
  self.cmds.isEmpty

protected def toString (self : Shim) : String :=
  self.text.source

instance : ToString Shim := ⟨Shim.toString⟩

/--
Add a command to the shim.
Fails if the command could not be reprinted.
-/
def pushCmd? (cmd : Cmd) (self : Shim) : Option Shim := do
  let startPos := self.text.source.endPos
  let code := self.text.source ++ (← cmd.raw.reprint).trim ++ "\n"
  return ⟨self.cmds.push ⟨cmd, startPos, code.endPos⟩, FileMap.ofString code⟩

/--
Appends an `Array` of commands to the shim.
Fails if any of the commands could not be reprinted and throws the command.
-/
def appendCmds? (cmds : Array Cmd) (self : Shim) : Except Cmd Shim := do
  let mut shimCmds := self.cmds
  let mut code := self.text.source
  for cmd in cmds do
    let startPos := code.endPos
    let some cmdCode := cmd.raw.reprint | throw cmd
    code := code ++ cmdCode.trim ++ "\n"
    shimCmds := shimCmds.push ⟨cmd, startPos, code.endPos⟩
  return ⟨shimCmds, FileMap.ofString code⟩

/--
Create a shim from an `Array` of commands.
Returns an empty shim if any of the commands could not be reprinted.
-/
def ofCmds (cmds : Array Cmd) : Shim :=
  Shim.empty.appendCmds? cmds |>.toOption.getD Shim.empty

/--
Find the C source position within the shim
corresponding to the `leanPos` in the Lean source.
-/
def findPos? (self : Shim) (leanPos : String.Pos) : Option String.Pos :=
  self.cmds.findSome? (·.findPos? leanPos)
