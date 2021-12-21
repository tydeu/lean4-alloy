import Alloy.ParserUtil

namespace Alloy.C

/-!
A Lean DSL encoding the standard C syntax.

Modeled off of Microsoft's
[C Language Syntax Summary](https://docs.microsoft.com/en-us/cpp/c-language/c-language-syntax-summary)
and the C11 standard's
[specification](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf#page=476).
-/

--------------------------------------------------------------------------------
-- Type Forward Declaration
--------------------------------------------------------------------------------

-- a type qualifier
declare_syntax_cat cTypeQ (behavior := symbol)

-- a type specifier
declare_syntax_cat cTypeSpec (behavior := symbol)

-- a specifier-qualifier (and/or alignment specifier)
declare_syntax_cat cSpec (behavior := symbol)

syntax cTypeQ : cSpec
syntax cTypeSpec : cSpec

-- a declarator
declare_syntax_cat cDecl

-- an abstract declarator
declare_syntax_cat cADecl

syntax type := cSpec+ optional(cADecl)

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

declare_syntax_cat cExpr (behavior := symbol)

-- primary expression
syntax:max ident : cExpr
syntax:max num : cExpr
syntax:max char : cExpr
syntax:max str : cExpr
syntax:max "(" cExpr ")" : cExpr

-- postfix expression
syntax:1000 cExpr:1000 noWs "[" cExpr "]" : cExpr
syntax:1000 cExpr:1000 noWs "(" cExpr,* ")" : cExpr
syntax:1000 cExpr:1000 noWs "." noWs ident : cExpr
syntax:1000 cExpr:1000 noWs "->" noWs ident : cExpr
syntax:1000 cExpr:1000 noWs "++" : cExpr
syntax:1000 cExpr:1000 noWs "--" : cExpr

-- unary expression
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

-- cast expression
syntax:100 (name := castExpr) "(" type ")" cExpr:100 : cExpr

-- multiplicative expression
syntax:70 cExpr:70 " * " cExpr:71 : cExpr
syntax:70 cExpr:70 " / " cExpr:71 : cExpr
syntax:70 cExpr:70 " % " cExpr:71 : cExpr

-- additive expression
syntax:65 cExpr:65 " + " cExpr:66 : cExpr
syntax:65 cExpr:65 " - " cExpr:66 : cExpr

-- shift expression
syntax:60 cExpr:60 " << " cExpr:61 : cExpr
syntax:60 cExpr:60 " >> " cExpr:61 : cExpr

-- relational expression
syntax:55 cExpr:55 " < " cExpr:56 : cExpr
syntax:55 cExpr:55 " > " cExpr:56 : cExpr
syntax:55 cExpr:55 " <= " cExpr:56 : cExpr
syntax:55 cExpr:55 " >= " cExpr:56 : cExpr

-- equality expression
syntax:50 cExpr:50 " == " cExpr:51 : cExpr
syntax:50 cExpr:50 " != " cExpr:51 : cExpr

-- bitwise expression
syntax:45 cExpr:45 " & " cExpr:46 : cExpr
syntax:43 cExpr:43 " ^ " cExpr:44 : cExpr
syntax:40 cExpr:40 " | " cExpr:41 : cExpr

-- logical expression
syntax:35 cExpr:35 " && " cExpr:36 : cExpr
syntax:30 cExpr:30 " || " cExpr:31 : cExpr

-- conditional expression
syntax:25 (name := condExpr) cExpr:26 " ? " cExpr " : " cExpr:25 : cExpr

-- assignment expression
syntax:20 cExpr:500 " = " cExpr:20 : cExpr
syntax:20 cExpr:500 " *= " cExpr:20 : cExpr
syntax:20 cExpr:500 " /= " cExpr:20 : cExpr
syntax:20 cExpr:500 " %= " cExpr:20 : cExpr
syntax:20 cExpr:500 " += " cExpr:20 : cExpr
syntax:20 cExpr:500 " -= " cExpr:20 : cExpr
syntax:20 cExpr:500 " <<= " cExpr:20 : cExpr
syntax:20 cExpr:500 " >>= " cExpr:20 : cExpr
syntax:20 cExpr:500 " &= " cExpr:20 : cExpr
syntax:20 cExpr:500 " ^= " cExpr:20 : cExpr
syntax:20 cExpr:500 " |= " cExpr:20 : cExpr

-- constant expression
def constExpr := condExpr

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

declare_syntax_cat cDeclSpec

syntax cSpec : cDeclSpec

declare_syntax_cat cStorageClassSpec (behavior := symbol)

syntax "auto" : cStorageClassSpec
syntax "extern" : cStorageClassSpec
syntax "register" : cStorageClassSpec
syntax "static" : cStorageClassSpec
syntax "_Thread_local" : cStorageClassSpec
syntax "typedef" : cStorageClassSpec

syntax cStorageClassSpec : cDeclSpec

declare_syntax_cat cFunSpec (behavior := symbol)

syntax "inline" : cFunSpec
syntax "_Noreturn" : cFunSpec

syntax cFunSpec : cDeclSpec

declare_syntax_cat cIndex

syntax cExpr : cIndex
syntax "static" cTypeQ* cExpr : cIndex
syntax cTypeQ+ "static"? cExpr : cIndex
syntax cTypeQ* "*" : cIndex

syntax paramDecl := cDeclSpec+ optional(cDecl <|> cADecl)
syntax params := paramDecl,+,? "..."?

syntax:max ident : cDecl
syntax:max "(" cDecl ")" : cDecl
syntax:arg cDecl:arg "[" «cIndex»? "]" : cDecl
syntax:arg cDecl:arg "(" params ")" : cDecl
syntax:arg cDecl:arg "(" ident* ")" : cDecl
syntax:lead "*" cTypeQ* cDecl:lead : cDecl

syntax:max "*" cTypeQ* : cADecl
syntax:max "(" cADecl ")" : cADecl
syntax:max "(" params ")" : cADecl
syntax:arg cADecl:arg "[" «cIndex»? "]" : cADecl
syntax:arg cADecl:arg "(" params ")" : cADecl
syntax:lead "*" cTypeQ* cADecl:lead : cADecl

declare_syntax_cat cDesignator
declare_syntax_cat cInitializer

syntax "[" constExpr "]" : cDesignator
syntax "." ident : cDesignator

syntax cExpr : cInitializer
syntax "{" (optional(cDesignator+ "=") cInitializer),+,? "}" : cInitializer

syntax initDeclarator := cDecl optional(" = " cInitializer)
syntax declaration := cDeclSpec+ initDeclarator,* ";"

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Qualifiers

syntax "const" : cTypeQ
syntax "restrict" : cTypeQ
syntax "volatile" : cTypeQ
syntax "_Atomic" : cTypeQ

-- Primitives

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

-- Atomic

syntax "_Atomic" "(" type ")" : cTypeSpec

-- Struct/Union

syntax aggrDeclBits := " : " constExpr
syntax aggrDeclarator := aggrDeclBits <|> cDecl optional(aggrDeclBits)
syntax aggrDeclaration := cSpec+ aggrDeclarator,* ";"
syntax aggrSpecDef := "{" aggrDeclaration* "}"
syntax aggrSpec := aggrSpecDef <|> ident optional(aggrSpecDef)

syntax:max "struct " aggrSpec : cTypeSpec
syntax:max "union " aggrSpec : cTypeSpec

-- Enum

syntax enumerator := ident optional(" = " constExpr)
syntax enumSpecDef := "{" enumerator,+ "}"
syntax enumSpec := enumSpecDef <|> ident optional(enumSpecDef)

syntax:max "enum " enumSpec : cTypeSpec

-- Alignment

syntax (name := alignSpec) "_Alignas" "(" (type <|> constExpr) ")" : cSpec

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

declare_syntax_cat cStmt (behavior := symbol)

-- jump statement
syntax (name := gotoStmt) "goto" ident ";" : cStmt
syntax (name := continueStmt) "continue" ";" : cStmt
syntax (name := breakStmt) "break" ";" : cStmt
syntax (name := returnStmt) "return" cExpr,* ";" : cStmt

-- compound statement
syntax (name := compStmt) "{" declaration* cStmt* "}" : cStmt

-- expression statement
syntax (name := exprStmt) cExpr,+ ";" : cStmt

-- iteration statement
syntax (name := whileStmt) "while " "(" cExpr,+ ")" cStmt : cStmt
syntax (name := doWhileStmt) "do " cStmt " while " "(" cExpr,+ ")" : cStmt
syntax (name := forStmt) "for " "(" cExpr,* ";" cExpr,* ";" cExpr,* ")" cStmt : cStmt

-- selection statement
syntax (name := ifStmt) "if " "(" cExpr,+ ")" cStmt (" else " cStmt)? : cStmt
syntax (name := switchStmt) "switch " "(" cExpr,+ ")" cStmt : cStmt

-- labeled statement
syntax (name := labelStmt) ident ": " cStmt : cStmt
syntax (name := caseStmt) "case " constExpr ": " cStmt : cStmt
syntax (name := defaultStmt) "default" ": " cStmt : cStmt

--------------------------------------------------------------------------------
-- Top-Level Commands
--------------------------------------------------------------------------------

declare_syntax_cat cCmd

-- Top-Level Definitions

syntax function := cDeclSpec+ cDecl declaration* compStmt

syntax function : cCmd
syntax declaration : cCmd

-- Preprocessor Commands

declare_syntax_cat ppCmd

syntax ppCmd : cCmd

@[runParserAttributeHooks]
def angleHeaderName := rawUntilCh '>'

syntax angleHeader := "<" angleHeaderName ">"
syntax header := str <|> angleHeader

syntax "#include " header linebreak : ppCmd
syntax "#define " ident (noWs "("  ident,*,?  "..."? ")")? line linebreak : ppCmd
syntax "#undef " ident linebreak : ppCmd
syntax "#line " line linebreak : ppCmd
syntax "#error " line linebreak : ppCmd
syntax "#pragma " line linebreak : ppCmd
syntax "#" linebreak : ppCmd

syntax "#if " constExpr linebreak : ppCmd
syntax "#ifdef " ident linebreak : ppCmd
syntax "#ifndef " ident linebreak : ppCmd
syntax "#elif " constExpr linebreak : ppCmd
syntax "#else" linebreak : ppCmd
syntax "#endif" linebreak : ppCmd
