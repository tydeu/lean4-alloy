/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Grammar

/-!
# C Syntax Utilities

Definitions for dealing with C-specific syntax.
-/

open Lean
namespace Alloy.C

/-!
## Typed Syntax Helpers
-/

/-! ### Categories -/

abbrev Cmd := TSyntax `cCmd
abbrev PPCmd := TSyntax `ppCmd
abbrev ExternDecl := TSyntax `cExternDecl
abbrev DirectAbsDeclarator := TSyntax `cDirectAbsDeclarator
abbrev DirectDeclarator := TSyntax `cDirectDeclarator
abbrev Index := TSyntax `cIndex
abbrev DeclSpec := TSyntax `cDeclSpec
abbrev StorageClassSpec := TSyntax `cStorageClassSpec
abbrev FunSpec := TSyntax `cFunSpec
abbrev Spec := TSyntax `cSpec
abbrev TypeSpec := TSyntax `cTypeSpec
abbrev TypeQ := TSyntax `cTypeQ
abbrev Designator := TSyntax `cDesignator
abbrev Initializer := TSyntax `cInitializer
abbrev Expr := TSyntax `cExpr
abbrev AssignOp := TSyntax `cAssignOp
abbrev Stmt := TSyntax `cStmt

/-! ### Kinds -/

abbrev Pointer := TSyntax ``pointer
abbrev AlignSpec := TSyntax ``alignSpec
abbrev ParamDecl := TSyntax ``paramDecl
abbrev Params := TSyntax ``params
abbrev Function := TSyntax ``function
abbrev Declarator := TSyntax ``declarator
abbrev AbsDeclarator := TSyntax ``absDeclarator
abbrev AggrDeclarator := TSyntax ``aggrDeclarator
abbrev AggrSig := TSyntax ``aggrSig
abbrev Declaration := TSyntax ``declaration
abbrev CompStmt := TSyntax ``compStmt

/-! ### Coercions -/

instance : Coe Ident Expr where
  coe x := Unhygienic.run `(cExpr| $x:ident)

instance : Coe NumLit Expr where
  coe x := Unhygienic.run `(cExpr| $x:num)

instance : Coe ScientificLit Expr where
  coe x := Unhygienic.run `(cExpr| $x:scientific)

instance : Coe StrLit Expr where
  coe x := Unhygienic.run `(cExpr| $x:str)

instance : Coe CharLit Expr where
  coe x := Unhygienic.run `(cExpr| $x:char)

instance : Coe Expr Initializer where
  coe x := Unhygienic.run `(cInitializer| $x:cExpr)

instance : Coe Expr Index where
  coe x := Unhygienic.run `(cIndex| $x:cExpr)

instance : Coe Ident DirectDeclarator where
  coe x := Unhygienic.run `(cDirectDeclarator| $x:ident)

instance : Coe DirectDeclarator Declarator where
  coe x := Unhygienic.run `(declarator| $x:cDirectDeclarator)

instance : Coe Declarator AggrDeclarator where
  coe x := Unhygienic.run `(aggrDeclarator| $x:declarator)

instance : Coe Pointer AbsDeclarator where
  coe x := Unhygienic.run `(absDeclarator| $x:pointer)

instance : Coe DirectAbsDeclarator AbsDeclarator where
  coe x := Unhygienic.run `(absDeclarator| $x:cDirectAbsDeclarator)

instance : Coe Ident AggrSig where
  coe x := Unhygienic.run `(aggrSig| $x:ident)

instance : Coe Ident TypeSpec where
  coe x := Unhygienic.run `(cTypeSpec| $x:ident)

instance : Coe TypeQ Spec where
  coe x := Unhygienic.run `(cSpec| $x:cTypeQ)

instance : Coe TypeSpec Spec where
  coe x := Unhygienic.run `(cSpec| $x:cTypeSpec)

instance : Coe AlignSpec Spec where
  coe x := Unhygienic.run `(cSpec| $x:alignSpec)

instance : Coe Spec DeclSpec where
  coe x := Unhygienic.run `(cDeclSpec| $x:cSpec)

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
## Other Helpers
-/

def packBody : Stmt → CompStmt
| `(cStmt| $x:compStmt) => x
| stmt => Unhygienic.run `(compStmt| {$stmt:cStmt})
