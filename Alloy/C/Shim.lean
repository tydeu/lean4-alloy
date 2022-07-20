/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Syntax

open Lean

namespace Alloy.C

/--
A piece of shim `Syntax` that has been reprinted via `reprintShim` and thus its
nodes are tagged with synthetic `SourceInfo` detailing its span within the shim.
-/
abbrev ShimSyntax := Syntax

/-- Converts all non-whitespace characters of the string to whitespace. -/
def toWhitespace (str : String) : String :=
  str.map fun c => if c.isWhitespace then c else ' '

/-- `Lean.Syntax.reprintLeaf`, but transforms comments into whitespace. -/
def reprintLeaf (val : String) : SourceInfo → String
| .original lead _ trail _ =>
  s!"{toWhitespace lead.toString}{val}{toWhitespace trail.toString}"
| _  => s!" {val} "

/--
Like `Lean.Syntax.reprint`, but transforms comments into whitespace and
tags nodes with `SourceInfo` noting their position in the reprinted code.
-/
partial def reprint (stx : Syntax) (startPos : String.Pos := 0) : Option (String × ShimSyntax) := do
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
      s := s ++ s0
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
  | _  => val.bsize + 2

/--
Find the position within the `ShimSyntax`
corresponding to the `leanPos` in the Lean source.

The algorithm Traverses the syntax tree top-to-bottom, left-to-right to find
a leaf whose range includes `leanPos`. It assumes nothing about the relationship
between Lean source information of elements within the tree. Thus, it is very
linear in its performance.
-/
partial def ShimSyntax.leanPosToShim? (stx : ShimSyntax)
(leanPos : String.Pos) (shimStartPos : String.Pos := 0) : Option String.Pos := do
  if let .node info _ args := stx then
    --
    let .synthetic shimStartPos _ := info | failure
    let mut startPos := shimStartPos
    for arg in args do
      if let some pos := leanPosToShim? arg leanPos startPos then
        return pos
      startPos := startPos + ⟨ShimSyntax.bsize arg⟩
    failure
  else
    let some leanHeadPos := stx.getPos? | failure
    let some leanTailPos := stx.getTailPos? | failure
    if leanHeadPos ≤ leanPos && leanPos < leanTailPos then
      return leanPos - leanHeadPos + shimStartPos
    failure

/-- Computes the start and end of the `atom`/`ident` in the Lean source. -/
def ShimSyntax.leafRange? : ShimSyntax → Option (String.Pos × String.Pos)
| .atom (.original _ head _ tail) .. => (head, tail)
| .atom (.synthetic head tail) .. => (head, tail)
| .ident (.original _ head _ tail) .. => (head, tail)
| .ident (.synthetic head tail) .. => (head, tail)
| _ => none

/--
Find the Lean position within the
`ShimSyntax` corresponding to `shimPos` in the shim.

This algorithms assumes the shim `SourceInfo` position of nodes are
monotonically increasing from left-to-right and lower node's range is a subset
of a higher node's range in the syntax tree. In other words, it assumes the
syntax has not been re-ordered since it was tagged with source information
(e.g., via `reprint`).
-/
partial def ShimSyntax.shimPosToLean? (stx : ShimSyntax)
(shimPos : String.Pos) (shimHeadPos shimTailPos : String.Pos := 0) : Option String.Pos := do
  if let .node info _ args := stx then
    let .synthetic shimHeadPos shimTailPos := info | failure
    if shimHeadPos ≠ shimTailPos then -- e.g., null nodes
      if shimHeadPos > shimPos then failure
      if shimTailPos ≤ shimPos then failure
    let mut headPos := shimHeadPos
    for arg in args do
      let tailPos := headPos + ⟨ShimSyntax.bsize arg⟩
      if let some pos := shimPosToLean? arg shimPos headPos tailPos then
        return pos
      headPos := tailPos
  else
    let (leanHeadPos, leanTailPos) ← stx.leafRange?
    if shimHeadPos ≤ shimPos && shimPos < shimTailPos then
      let shimOff := shimPos - shimHeadPos
      let leanLen := leanTailPos - leanHeadPos
      let leanPos := leanHeadPos + min shimOff leanLen
      return leanPos
  failure

/-- A single, reprinted command in a C shim. -/
abbrev ShimCmd := Cmd

/-- A C shim -- an array of commands with shim source position information. -/
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

/--
Add a command to the shim.
Fails if the command could not be reprinted.
-/
def pushCmd? (cmd : Cmd) (self : Shim) : Option Shim := do
  let (code, cmd) ← reprint cmd.raw self.text.source.endPos
  let code := self.text.source ++ code ++ "\n"
  return ⟨self.cmds.push cmd, FileMap.ofString code⟩

/--
Appends an `Array` of commands to the shim.
Fails if any of the commands could not be reprinted and throws the command.
-/
def appendCmds? (cmds : Array Cmd) (self : Shim) : Except Cmd Shim := do
  let mut shimCmds := self.cmds
  let mut code := self.text.source
  for cmd in cmds do
    let some (cmdCode, cmd) := reprint cmd.raw code.endPos
      | throw cmd
    code := code ++ cmdCode ++ "\n"
    shimCmds := shimCmds.push cmd
  return ⟨shimCmds, FileMap.ofString code⟩

/--
Create a shim from an `Array` of commands.
Returns an empty shim if any of the commands could not be reprinted.
-/
def ofCmds (cmds : Array Cmd) : Shim :=
  Shim.empty.appendCmds? cmds |>.toOption.getD Shim.empty

/--
Find the position within the C shim
corresponding to the `leanPos` in the Lean source.
-/
def leanPosToC? (self : Shim) (leanPos : String.Pos) : Option String.Pos :=
  self.cmds.findSome? (·.leanPosToShim? leanPos)

/--
Find the position within the shim's Lean syntax
corresponding to the `cPos` in the C shim.
-/
def cPosToLean? (self : Shim) (cPos : String.Pos) : Option String.Pos :=
  self.cmds.findSome? (·.shimPosToLean? cPos)
