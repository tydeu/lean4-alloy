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
abbrev DirAbsDtor := TSyntax `cDirAbsDe
abbrev DirDtor := TSyntax `cDirDtor
abbrev Stmt := TSyntax `cStmt

abbrev Pointer := TSyntax ``pointer
abbrev ParamDecl := TSyntax ``paramDecl
abbrev Params := TSyntax ``params
abbrev Function := TSyntax ``function
abbrev Declaration := TSyntax ``declaration
abbrev CompStmt := TSyntax ``compStmt

instance : Coe Ident TypeSpec where
  coe s := ⟨s.raw⟩

instance : Coe TypeSpec DeclSpec where
  coe s := ⟨s.raw⟩

instance : Coe StorageClassSpec DeclSpec where
  coe s := ⟨s.raw⟩

instance : Coe FunSpec DeclSpec where
  coe s := ⟨s.raw⟩

instance : Coe CompStmt Stmt where
  coe s := ⟨s.raw⟩

instance : Coe Function ExternDecl where
  coe s := ⟨s.raw⟩

instance : Coe Declaration ExternDecl where
  coe s := ⟨s.raw⟩

instance : Coe ExternDecl Cmd where
   coe s := ⟨s.raw⟩

instance : Coe PPCmd Cmd where
   coe s := ⟨s.raw⟩

/-!
## Other Utilities
-/

def unpackStmtBody : Stmt → Syntax × Array Declaration × Array Stmt × Syntax
| `(compStmt|{%$head $decls* $[$stmts:cStmt]* }%$tail) => (head, decls, stmts, tail)
| stmt => (Syntax.missing, #[], #[stmt], Syntax.missing)
