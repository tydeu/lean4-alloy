/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Grammar

open Lean
namespace Alloy.C

/-!
## Typed Syntax Definitions
-/

abbrev Cmd := TSyntax `cCmd
abbrev PPCmd := TSyntax `ppCmd
abbrev ExternDecl := TSyntax `cExternDecl
abbrev TypeQ := TSyntax `cTypeQ
abbrev TypeSpec := TSyntax `cTypeSpec
abbrev DeclSpec := TSyntax `cDeclSpec
abbrev Expr := TSyntax `cExpr
abbrev StorageClassSpec := TSyntax `cStorageClassSpec
abbrev FunSpec := TSyntax `cFunSpec
abbrev DirAbsDtor := TSyntax `cDirAbsDtor
abbrev DirDtor := TSyntax `cDirDtor
abbrev Stmt := TSyntax `cStmt

abbrev Pointer := TSyntax ``pointer
abbrev ParamDecl := TSyntax ``paramDecl
abbrev Params := TSyntax ``params
abbrev Function := TSyntax ``function
abbrev Declaration := TSyntax ``declaration
abbrev CompStmt := TSyntax ``compStmt

instance : Coe Ident TypeSpec where
  coe x := Unhygienic.run `(cTypeSpec| $x:ident)

instance : Coe TypeSpec DeclSpec where
  coe x := Unhygienic.run `(cDeclSpec| $x:cTypeSpec)

instance : Coe StorageClassSpec DeclSpec where
  coe x := Unhygienic.run `(cDeclSpec| $x:cStorageClassSpec)

instance : Coe FunSpec DeclSpec where
  coe x := Unhygienic.run `(cDeclSpec| $x:cFunSpec)

instance : Coe CompStmt Stmt where
  coe x := Unhygienic.run `(cStmt| $x:compStmt)

instance : Coe Function ExternDecl where
  coe x := Unhygienic.run `(cExternDecl| $x:function)

instance : Coe Declaration ExternDecl where
  coe x := Unhygienic.run `(cExternDecl| $x:declaration)

instance : Coe ExternDecl Cmd where
  coe x := Unhygienic.run `(cCmd| $x:cExternDecl)

instance : Coe PPCmd Cmd where
  coe x := Unhygienic.run `(cCmd| $x:ppCmd)

/-!
## Other Utilities
-/

def packBody : Stmt → CompStmt
| `(cStmt| $x:compStmt) => x
| stmt => Unhygienic.run `(compStmt| {$stmt:cStmt})
