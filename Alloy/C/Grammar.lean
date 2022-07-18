/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.Util.Parser

/-!
# The C Grammar

This module contains a a Lean DSL that encodes the standard C syntax.

It uses Microsoft's [C Language Syntax Summary][1] and the C11 standard's
[specification][2] as references.

[1]: https://docs.microsoft.com/en-us/cpp/c-language/c-language-syntax-summary
[2]: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf#page=476
-/

namespace Alloy.C

--------------------------------------------------------------------------------
/-!
## Type Abstraction

We need to forward declare these in order to construct the syntax `type`
which is used by the cast expressions (`castExpr`) which is part of a constant
expression (`constExpr`) which is, in term, used in aggregate types
(e.g., `struct`).

A good reference on C representation of types is:
https://blog.robertelder.org/building-a-c-compiler-type-system-a-canonical-type-representation/
-/
--------------------------------------------------------------------------------

-- Encodes a `type-qualifier` of the C grammar.
declare_syntax_cat cTypeQ (behavior := symbol)

-- Encodes a `type-specifier` of the C grammar.
declare_syntax_cat cTypeSpec (behavior := symbol)

/-
Encodes a `specifier-qualifier` of the C grammar
(which includes an `alignment-specifier` in Microsoft's C standard)
-/
declare_syntax_cat cSpec (behavior := symbol)

syntax cTypeQ : cSpec
syntax cTypeSpec : cSpec

/-- Encodes a `pointer` of the C grammar. -/
syntax pointer := (" * " cTypeQ*)+

-- Encodes a `direct-declarator` of the C grammar.
declare_syntax_cat cDirDtor (behavior := symbol)

-- Encodes a `direct-abstract-declarator` of the C grammar.
declare_syntax_cat cDirAbsDtor (behavior := symbol)

-- Encodes an `declarator` of the C grammar.
syntax declarator := «pointer»? cDirDtor

-- Encodes an `abstract-declarator` of the C grammar.
syntax absDeclarator := pointer optional(cDirAbsDtor) <|> cDirAbsDtor

/-- Encodes an `type` of the C grammar. -/
syntax type := cSpec+ optional(absDeclarator)

--------------------------------------------------------------------------------
/-! ## Expressions                                                            -/
--------------------------------------------------------------------------------

/-
Encodes a single (not comma separated) `expression` of the C grammar.
That is, it encodes the formal  `assignment-expression` of the C grammar.
-/
declare_syntax_cat cExpr (behavior := symbol)

/-!
### Primary Expressions

Collectively (partially) encode a `primary-expression` of the C grammar.
-/

syntax:max ident : cExpr
syntax:max num : cExpr
syntax:max char : cExpr
syntax:max str : cExpr
syntax:max "(" cExpr ")" : cExpr

/-!
### Postfix Expressions

Collectively encode a `postfix-expression` of the C grammar.
-/

syntax:1000 cExpr:1000 noWs "[" cExpr "]" : cExpr
syntax:1000 cExpr:1000 noWs "(" cExpr,* ")" : cExpr
syntax:1000 cExpr:1000 noWs "." noWs ident : cExpr
syntax:1000 cExpr:1000 noWs "->" noWs ident : cExpr
syntax:1000 cExpr:1000 noWs "++" : cExpr
syntax:1000 cExpr:1000 noWs "--" : cExpr

/-!
### Unary Expressions

Collectively encode a `unary-expression` of the C grammar.
-/

syntax:500 "++" noWs cExpr:500 : cExpr
syntax:500 "--" noWs cExpr:500 : cExpr
syntax:500 "&" noWs cExpr:100 : cExpr
syntax:500 "*" noWs cExpr:100 : cExpr
syntax:500 "+" noWs cExpr:100 : cExpr
syntax:500 "-" noWs cExpr:100 : cExpr
syntax:500 "~" noWs cExpr:100 : cExpr
syntax:500 "!" noWs cExpr:100 : cExpr
syntax:500 "sizeof" cExpr:500 : cExpr
syntax:500 "sizeof" "(" cSpec ")" &" _Alignof" "(" cSpec ")" : cExpr


/-!
### Cast Expression
-/

/-- Encodes a `cast-expression` of the C grammar. -/
syntax:100 (name := castExpr) "(" type ")" cExpr:100 : cExpr

/-!
### Multiplicative Expressions

Collectively encode a `multiplicative-expression` of the C grammar.
-/

syntax:70 (name := mulExpr) cExpr:70 " * " cExpr:71 : cExpr
syntax:70 (name := divExpr) cExpr:70 " / " cExpr:71 : cExpr
syntax:70 (name := modExpr) cExpr:70 " % " cExpr:71 : cExpr

/-!
### Additive Expressions

Collectively encode an `additive-expression` of the C grammar.
-/

syntax:65 (name := addExpr) cExpr:65 " + " cExpr:66 : cExpr
syntax:65 (name := subExpr) cExpr:65 " - " cExpr:66 : cExpr

/-!
### Shift Expressions

Collectively encode a `shift-expression` of the C grammar.
-/

syntax:60 (name := shlExpr) cExpr:60 " << " cExpr:61 : cExpr
syntax:60 (name := shrExpr) cExpr:60 " >> " cExpr:61 : cExpr

/-!
### Relational Expressions

Collectively encode a `relational-expression` of the C grammar.
-/

syntax:55 (name := ltExpr) cExpr:55 " < " cExpr:56 : cExpr
syntax:55 (name := gtExpr) cExpr:55 " > " cExpr:56 : cExpr
syntax:55 (name := leExpr) cExpr:55 " <= " cExpr:56 : cExpr
syntax:55 (name := geExpr) cExpr:55 " >= " cExpr:56 : cExpr


/-!
### Equality Expressions

Collectively encode an `equality-expression` of the C grammar.
-/

syntax:50 (name := eqExpr) cExpr:50 " == " cExpr:51 : cExpr
syntax:50 (name := neExpr) cExpr:50 " != " cExpr:51 : cExpr

/-!
### Bitwise Expressions
-/

/-- Encodes an `AND-expression` of the grammar. -/
syntax:45 (name := andExpr) cExpr:45  " & " cExpr:46 : cExpr

/-- Encodes an `exclusive-OR-expression` of the grammar. -/
syntax:43 (name := xorExpr) cExpr:43  " ^ " cExpr:44 : cExpr

/-- Encodes an `inclusive-OR-expression` of the grammar. -/
syntax:40 (name := orExpr) cExpr:40 " | " cExpr:41 : cExpr

/-!
### Logical Expressions
-/

/-- Encodes a `logical-AND-expression` of the grammar. -/
syntax:35 (name := logicalAndExpr) cExpr:35 " && " cExpr:36 : cExpr

/-- Encodes a `logical-OR-expression` of the grammar. -/
syntax:30 (name := logicalOrExpr) cExpr:30 " || " cExpr:31 : cExpr

/-!
### Conditional Expression
-/

/- Encodes a `conditional-expression` of the C grammar. -/
syntax:20 (name := condExpr) cExpr:21 " ? " cExpr,+ " : " cExpr:20 : cExpr

/-!
### Assignment Expression
-/

/-- Encodes an `assignment-operator` of the C grammar. -/
syntax assignOp :=
  " = " <|> " *= " <|> " /= " <|> " %= " <|> " += " <|> " -= " <|>
  " <<= " <|> " >>= " <|> " &= " <|> " ^= " <|> " |= "

/-- Encodes a non-conditional `assignment-expression` of the C grammar. -/
syntax:20 (name := assignExpr) cExpr:500 assignOp cExpr:20 : cExpr

/-!
### Constant Expression
-/

/-- Encodes a `constant-expression` of the C grammar. -/
def constExpr := condExpr

--------------------------------------------------------------------------------
/-! ## Declaration Syntax                                                     -/
--------------------------------------------------------------------------------

/-!
### Specifiers
-/

-- Encodes a `declaration-specifier` of the C grammar.
declare_syntax_cat cDeclSpec

syntax cSpec : cDeclSpec

-- Encodes a `storage-class-specifier` of the C grammar.
declare_syntax_cat cStorageClassSpec (behavior := symbol)

syntax "auto" : cStorageClassSpec
syntax "extern" : cStorageClassSpec
syntax "register" : cStorageClassSpec
syntax "static" : cStorageClassSpec
syntax "_Thread_local" : cStorageClassSpec
syntax "typedef" : cStorageClassSpec

syntax cStorageClassSpec : cDeclSpec

-- Encodes a `function-specifier` of the C grammar.
declare_syntax_cat cFunSpec (behavior := symbol)

syntax "inline" : cFunSpec
syntax "_Noreturn" : cFunSpec

syntax cFunSpec : cDeclSpec

/-!
### Declarators
-/

declare_syntax_cat cIndex

syntax cExpr : cIndex
syntax "static" cTypeQ* cExpr : cIndex
syntax cTypeQ+ "static"? cExpr : cIndex
syntax cTypeQ* "*" : cIndex

/-- Encodes a `parameter-declaration` of the C grammar. -/
syntax paramDecl := cDeclSpec+ (declarator <|> absDeclarator)?

/-- Encodes a `parameter-type-list` of the C grammar. -/
syntax params := paramDecl,+,? "..."?

syntax:max ident : cDirDtor
syntax:max "(" declarator ")" : cDirDtor
syntax:arg cDirDtor:arg "[" optional(cIndex)"]" : cDirDtor
syntax:arg cDirDtor:arg "(" params ")" : cDirDtor
syntax:arg cDirDtor:arg "(" ident* ")" : cDirDtor

syntax:max "(" absDeclarator ")" : cDirAbsDtor
syntax:max "(" params ")" : cDirAbsDtor
syntax:arg cDirAbsDtor:arg "[" optional(cIndex) "]" : cDirAbsDtor
syntax:arg cDirAbsDtor:arg "(" optional(params) ")" : cDirAbsDtor


/-!
### Declarations
-/

-- Encodes a `designator` of the C grammar.
declare_syntax_cat cDesignator

-- Encodes a `initializer` of the C grammar.
declare_syntax_cat cInitializer

syntax "[" constExpr "]" : cDesignator
syntax "." ident : cDesignator

syntax cExpr : cInitializer
syntax "{" (optional(cDesignator+ "=") cInitializer),+,? "}" : cInitializer

/-- Encodes an `init-declarator` of the C grammar. -/
syntax initDeclarator := declarator optional(" = " cInitializer)

/--
Encodes a `declaration` of the C grammar.

Note that an `ident` can be a `cDeclSpec` or `declarator`.
Lean will thus eagerly eat a sequence of `ident` up as solely a
sequence of  `cDeclSpec` and then fail to parse the declaration.
The lookahead ensures the last `ident` is parsed properly as a `declarator`.
-/
syntax declaration :=
  (atomic(lookahead(cDeclSpec (cDeclSpec <|> declarator <|> ";"))) cDeclSpec)+
  initDeclarator,* ";"

--------------------------------------------------------------------------------
/-! ## Types                                                                  -/
--------------------------------------------------------------------------------

/-!
### Qualifiers
-/

syntax "const" : cTypeQ
syntax "restrict" : cTypeQ
syntax "volatile" : cTypeQ
syntax "_Atomic" : cTypeQ

/-!
### Primitives
-/

syntax "void" : cTypeSpec
syntax "char" : cTypeSpec
syntax "short" : cTypeSpec
syntax "int" : cTypeSpec
syntax "long" : cTypeSpec
syntax "float" : cTypeSpec
syntax "double" : cTypeSpec
syntax "signed" : cTypeSpec
syntax "unsigned" : cTypeSpec

syntax "_Bool" : cTypeSpec
syntax "_Complex" : cTypeSpec

syntax ident : cTypeSpec

/-!
### Atomic
-/

/-- Encodes an `atomic-type-specifier` of the C grammar. -/
syntax (name := atomicSpec) "_Atomic" "(" type ")" : cTypeSpec

/-!
### Aggregates

Collective encodes the `struct-or-union-specifier` of the C grammar.
-/

syntax aggrDeclBits := " : " constExpr

/-- Encodes a `struct-declarator` of the C grammar. -/
syntax aggrDeclarator := aggrDeclBits <|> declarator optional(aggrDeclBits)

/-- Encodes a `struct-declaration` of the C grammar. -/
syntax aggrDeclaration := cSpec+ aggrDeclarator,* ";"

syntax aggrSigDef := "{" aggrDeclaration* "}"
syntax aggrSig := aggrSigDef <|> ident optional(aggrSigDef)

syntax (name := structSpec) "struct " aggrSig : cTypeSpec
syntax (name := unionSpec) "union " aggrSig : cTypeSpec

/-!
### Enums
-/

/-- Encodes an `enumerator` of the C grammar. -/
syntax enumerator := ident optional(" = " constExpr)

syntax enumSigDef := "{" enumerator,+ "}"
syntax enumSig := enumSigDef <|> ident optional(enumSigDef)

/-- Encodes an `enum-specifier` of the C grammar. -/
syntax (name := enumSpec) "enum " enumSig : cTypeSpec

/-!
### Alignment
-/

/-- Encodes a `alignment-specifier` of the C grammar. -/
syntax (name := alignSpec) "_Alignas" "(" (type <|> constExpr) ")" : cSpec

--------------------------------------------------------------------------------
/-! ## Statements                                                             -/
--------------------------------------------------------------------------------

-- Encodes a `statement` of the C grammar.
declare_syntax_cat cStmt (behavior := symbol)

/-!
### Jump Statements

Collectively encode a `jump-statement` of the C grammar.
-/

syntax (name := gotoStmt) "goto " ident ";" : cStmt
syntax (name := continueStmt) "continue" ";" : cStmt
syntax (name := breakStmt) "break" ";" : cStmt
syntax (name := returnStmt) "return" cExpr,* ";" : cStmt

/-!
### Compound Statement
-/

/-- Encodes an `compound-statement` of the C grammar. -/
syntax (name := compStmt) "{" declaration* cStmt* "}" : cStmt

/-!
### Expression Statement
-/

/-- Encodes an `expression-statement` of the C grammar. -/
syntax (name := exprStmt) cExpr,+ ";" : cStmt

/-!
### Iteration Statements

Collectively encode a `iteration-statement` of the C grammar.
-/

syntax (name := whileStmt) "while " "(" cExpr,+ ")" cStmt : cStmt
syntax (name := doWhileStmt) "do " cStmt " while " "(" cExpr,+ ")" : cStmt
syntax (name := forStmt) "for " "(" cExpr,* ";" cExpr,* ";" cExpr,* ")" cStmt : cStmt

/-!
### Selection Statements

Collectively encode a `selection-statement` of the C grammar.
-/

syntax (name := ifStmt) "if " "(" cExpr,+ ")" cStmt (" else " cStmt)? : cStmt
syntax (name := switchStmt) "switch " "(" cExpr,+ ")" cStmt : cStmt

/-!
### Labeled Statements

Collectively encode a `labeled-statement` of the C grammar.
-/

syntax (name := labelStmt) ident ": " cStmt : cStmt
syntax (name := caseStmt) "case " constExpr ": " cStmt : cStmt
syntax (name := defaultStmt) "default" ": " cStmt : cStmt

--------------------------------------------------------------------------------
/-! ## Top-Level Commands                                                     -/
--------------------------------------------------------------------------------

declare_syntax_cat cCmd

/-!
### External Declarations
-/

-- Encodes an `external-declaration` of the C grammar.
declare_syntax_cat cExternDecl

/--
Encodes a `function` of the C grammar.
See `declaration` as to why the lookahead is needed.
-/
syntax function :=
  (atomic(lookahead(cDeclSpec (cDeclSpec <|> declarator))) cDeclSpec)+
  declarator declaration* compStmt

syntax function : cExternDecl
syntax declaration : cExternDecl

syntax cExternDecl : cCmd

/-!
### Headers
-/

/-- Encodes an `h-char-sequence` of the C grammar. -/
@[runParserAttributeHooks] def angleHeaderName := rawUntilCh '>'

syntax angleHeader := "<" angleHeaderName ">"

/-- Encodes a `header-name` of the C grammar. -/
syntax header := str <|> angleHeader

/-!
### Preprocessor Commands
-/

declare_syntax_cat ppCmd

syntax ppCmd : cCmd

syntax "#include " header : ppCmd
syntax "#define " ident (noWs "("  ident,*,?  "..."? ")")? line : ppCmd
syntax "#undef " ident : ppCmd
syntax "#line " line : ppCmd
syntax "#error " line : ppCmd
syntax "#pragma " line : ppCmd
syntax "#" : ppCmd

syntax "#if " constExpr : ppCmd
syntax "#ifdef " ident : ppCmd
syntax "#ifndef " ident : ppCmd
syntax "#elif " constExpr : ppCmd
syntax "#else" : ppCmd
syntax "#endif" : ppCmd
