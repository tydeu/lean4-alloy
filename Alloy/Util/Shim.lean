/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Lean.Syntax
import Lean.Data.Position

open Lean

namespace Alloy

/--
A piece of shim `Syntax` that has been reprinted via `reprintShim` and thus its
nodes are tagged with synthetic `SourceInfo` detailing its span within the shim.
-/
abbrev ShimSyntax := Syntax

/-- The text of some shim syntax combined with tree representation. -/
abbrev ShimElem := String × ShimSyntax

/-- Converts all non-whitespace characters of the string to whitespace. -/
def toWhitespace (str : String) : String :=
  str.map fun c => if c.isWhitespace then c else ' '

/-- `Lean.Syntax.reprintLeaf`, but transforms comments into whitespace. -/
def reprintLeaf (val : String) : SourceInfo → String
| .original lead _ trail _ =>
  s!"{toWhitespace lead.toString}{val}{toWhitespace trail.toString}"
| _  => s!"{val} "

/--
Like `Lean.Syntax.reprint`, but transforms comments into whitespace and
tags nodes with `SourceInfo` noting their position in the reprinted code.
-/
partial def reprint (stx : Syntax) (startPos : String.Pos := 0) : Option ShimElem := do
  match stx with
  | .atom info val =>
    return (reprintLeaf val info, stx)
  | .ident info rawVal _ _ =>
    return (reprintLeaf rawVal.toString info, stx)
  | .node _ kind args =>
    let mut s := ""
    let mut args' := #[]
    if kind = choiceKind then
      let (s0, arg0) ← reprint (← args[0]?) startPos
      args' := args'.push arg0
      for arg in args[1:] do
        let (s', arg') ← reprint arg startPos
        args' := args'.push arg'
        guard (s0 == s')
      s := s0
    else
      for arg in args do
        let pos := startPos + s.endPos
        let (s', arg') ← reprint arg pos
        args' := args'.push arg'
        s := s ++ s'
    return (s, Syntax.node (.synthetic startPos (startPos + s.endPos)) kind args')
  | _ => failure

/-- Computes the `bsize` of the reprinted `Syntax` in the shim source. -/
def ShimSyntax.bsize : ShimSyntax → Nat
| .atom info val => sizeLeaf val.toSubstring info
| .ident info rawVal .. => sizeLeaf rawVal info
| .node (.synthetic head tail) _ _ => tail.byteIdx - head.byteIdx
| _ => 0
where
  sizeLeaf (val : Substring) : SourceInfo → Nat
  | .original lead _ trail _ => lead.bsize + val.bsize + trail.bsize
  | _  => val.bsize + 1

/--
Find the position within the `ShimSyntax`
corresponding to the `leanPos` in the Lean source.

The algorithm Traverses the syntax tree top-to-bottom, left-to-right to find
a leaf whose range includes `leanPos`. It assumes nothing about the relationship
between Lean source information of elements within the tree. Thus, it is very
linear in its performance.
-/
partial def ShimSyntax.leanPosToShim? (stx : ShimSyntax)
(leanPos : String.Pos) (includeStop := false) : Option String.Pos := do
  go stx 0 0
where
  go stx shimHeadPos shimTailPos := do
    if let .node info _ args := stx then
      let .synthetic shimHeadPos _ := info | failure
      let mut headPos := shimHeadPos
      for arg in args do
        let tailPos := headPos + ⟨ShimSyntax.bsize arg⟩
        if let some pos := go arg headPos tailPos then
          return pos
        headPos := tailPos
      failure
    else
      let leanRange ← stx.getRange?
      if leanRange.contains leanPos includeStop then
        let leanOff := leanPos - leanRange.start
        let shimLen := shimTailPos - shimHeadPos
        let shimPos := shimHeadPos + ⟨min leanOff.byteIdx shimLen.byteIdx⟩
        return shimPos
      failure

/--
Find the Lean `Syntax` leaf within the
`ShimSyntax` corresponding to `shimPos` in the shim.

This algorithms assumes the shim `SourceInfo` position of nodes are
monotonically increasing from left-to-right and lower node's range is a subset
of a higher node's range in the syntax tree. In other words, it assumes the
syntax has not been re-ordered since it was tagged with source information
(e.g., via `reprint`).
-/
partial def ShimSyntax.shimPosToLeanStx? (stx : ShimSyntax)
(shimPos : String.Pos) (includeStop := false) : Option Syntax :=
  go stx 0 0
where
  go stx shimHead shimTail := do
    if let .node info _ args := stx then
      let .synthetic shimHead shimTail := info | failure
      if shimHead ≠ shimTail then -- e.g., still enter null nodes
        guard <| String.Range.contains ⟨shimHead, shimTail⟩ shimPos includeStop
      let mut headPos := shimHead
      for arg in args do
        let tailPos := headPos + ⟨ShimSyntax.bsize arg⟩
        if let some stx := go arg headPos tailPos then
          return stx
        headPos := tailPos
    else
      if String.Range.contains ⟨shimHead, shimTail⟩ shimPos includeStop then
        return stx
    failure

/-- A shim -- an array of commands with shim source position information. -/
structure Shim where
  cmds : Array ShimSyntax
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

/-- Add some code verbatim to the shim. -/
@[inline] def addCode (code : String) (self : Shim) : Shim :=
  {self with text := FileMap.ofString <| self.text.source ++ code}

/-- Add a block of code to the shim. Appends a newline to the end. -/
@[inline] def addCodeBlock (code : String) (self : Shim) : Shim :=
  self.addCode <| code ++ "\n"

/--
Add some code verbatim to the shim.
**Does not update `FileMap` positions.**
-/
@[inline] def addCodeSnippet (code : String) (self : Shim) : Shim :=
  {self with text.source := self.text.source ++ code}

/--
Update the `FileMap` of the shim
(e.g., after a sequence of `addCodeSnippet` calls).
-/
@[inline] def updateFileMap (self : Shim) : Shim :=
  {self with text := FileMap.ofString self.text.source}

/-- Add a command syntax to the shim's syntax tree and rebuild the `FileMap`. -/
def addCmd (stx : Syntax) (self : Shim) : Shim :=
  ⟨self.cmds.push stx, FileMap.ofString (self.text.source ++ "\n")⟩

/--
Reprint a command and add it to the shim.
Fails if the command could not be reprinted.
-/
def pushCmd? (stx : Syntax) (self : Shim) : Option Shim := do
  let (code, stx) ← reprint stx self.text.source.endPos
  let code := self.text.source ++ code ++ "\n"
  return ⟨self.cmds.push stx, FileMap.ofString code⟩

/--
Reprint and append an `Array` of commands to the shim.
Fails if any of the commands could not be reprinted and throws the command.
-/
def appendCmds? (cmds : Array Syntax) (self : Shim) : Except Syntax Shim := do
  let mut shimCmds := self.cmds
  let mut code := self.text.source
  for cmd in cmds do
    let some (cmdCode, cmd) := reprint cmd code.endPos
      | throw cmd
    code := code ++ cmdCode ++ "\n"
    shimCmds := shimCmds.push cmd
  return ⟨shimCmds, FileMap.ofString code⟩

/--
Create a shim from an `Array` of commands.
Returns an empty shim if any of the commands could not be reprinted.
-/
def ofCmds (cmds : Array Syntax) : Shim :=
  Shim.empty.appendCmds? cmds |>.toOption.getD Shim.empty

/--
Find the position within the shim
corresponding to the `leanPos` in the Lean source.
-/
def leanPosToShim? (self : Shim) (leanPos : String.Pos) (includeStop := false) : Option String.Pos := do
  self.cmds.findSome? (·.leanPosToShim? leanPos includeStop)

/--
Find the `Syntax` leaf within the shim's Lean source
corresponding to the `shimPos` in the shim.
-/
def shimPosToLeanStx? (self : Shim) (shimPos : String.Pos) (includeStop := false) : Option Syntax :=
  self.cmds.findSome? (·.shimPosToLeanStx? shimPos includeStop)
