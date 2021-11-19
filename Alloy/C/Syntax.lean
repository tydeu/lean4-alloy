import Lean.Elab

namespace Alloy.C

/-
  C syntax is modeled off of Microsoft's
  [C Language Syntax Summary](https://docs.microsoft.com/en-us/cpp/c-language/c-language-syntax-summary).
-/

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

declare_syntax_cat cExpr

-- primary expression
scoped syntax:max ident : cExpr
scoped syntax:max num : cExpr
scoped syntax:max char : cExpr
scoped syntax:max str : cExpr
scoped syntax:max "(" cExpr ")" : cExpr

-- postfix expression
scoped syntax:1000 cExpr:1000 noWs "[" cExpr "]" : cExpr
scoped syntax:1000 cExpr:1000 noWs "(" cExpr,* ")" : cExpr
scoped syntax:1000 cExpr:1000 noWs "." noWs ident : cExpr
scoped syntax:1000 cExpr:1000 noWs "->" noWs ident : cExpr
scoped syntax:1000 cExpr:1000 noWs "++" : cExpr
scoped syntax:1000 cExpr:1000 noWs "--" : cExpr

-- unary expression
scoped syntax:500 "++" noWs cExpr:500 : cExpr
scoped syntax:500 "--" noWs cExpr:500 : cExpr
scoped syntax:500 "&" noWs cExpr:100 : cExpr
scoped syntax:500 "*" noWs cExpr:100 : cExpr
scoped syntax:500 "+" noWs cExpr:100 : cExpr
scoped syntax:500 "-" noWs cExpr:100 : cExpr
scoped syntax:500 "~" noWs cExpr:100 : cExpr
scoped syntax:500 "!" noWs cExpr:100 : cExpr
scoped syntax:500 "sizeof" cExpr:500 : cExpr

-- cast expression
scoped syntax:100 (name := castExpr) "(" ident ")" cExpr:100 : cExpr

-- multiplicative expression
scoped syntax:70 cExpr:70 " * " cExpr:71 : cExpr
scoped syntax:70 cExpr:70 " / " cExpr:71 : cExpr
scoped syntax:70 cExpr:70 " % " cExpr:71 : cExpr

-- additive expression
scoped syntax:65 cExpr:65 " + " cExpr:66 : cExpr
scoped syntax:65 cExpr:65 " - " cExpr:66 : cExpr

-- shift expression
scoped syntax:60 cExpr:60 " << " cExpr:61 : cExpr
scoped syntax:60 cExpr:60 " >> " cExpr:61 : cExpr

-- relational expression
scoped syntax:55 cExpr:55 " < " cExpr:56 : cExpr
scoped syntax:55 cExpr:55 " > " cExpr:56 : cExpr
scoped syntax:55 cExpr:55 " <= " cExpr:56 : cExpr
scoped syntax:55 cExpr:55 " >= " cExpr:56 : cExpr

-- equality expression
scoped syntax:50 cExpr:50 " == " cExpr:51 : cExpr
scoped syntax:50 cExpr:50 " != " cExpr:51 : cExpr

-- bitwise expression
scoped syntax:45 cExpr:45 " & " cExpr:46 : cExpr
scoped syntax:43 cExpr:43 " ^ " cExpr:44 : cExpr
scoped syntax:40 cExpr:40 " | " cExpr:41 : cExpr

-- logical expression
scoped syntax:35 cExpr:35 " && " cExpr:36 : cExpr
scoped syntax:30 cExpr:30 " || " cExpr:31 : cExpr

-- conditional expression
scoped syntax:25 (name := condExpr) cExpr:26 " ? " cExpr " : " cExpr:25 : cExpr

-- assignment expression
scoped syntax:20 cExpr:500 " = " cExpr:20 : cExpr
scoped syntax:20 cExpr:500 " *= " cExpr:20 : cExpr
scoped syntax:20 cExpr:500 " /= " cExpr:20 : cExpr
scoped syntax:20 cExpr:500 " %= " cExpr:20 : cExpr
scoped syntax:20 cExpr:500 " += " cExpr:20 : cExpr
scoped syntax:20 cExpr:500 " -= " cExpr:20 : cExpr
scoped syntax:20 cExpr:500 " <<= " cExpr:20 : cExpr
scoped syntax:20 cExpr:500 " >>= " cExpr:20 : cExpr
scoped syntax:20 cExpr:500 " &= " cExpr:20 : cExpr
scoped syntax:20 cExpr:500 " ^= " cExpr:20 : cExpr
scoped syntax:20 cExpr:500 " |= " cExpr:20 : cExpr

-- constant expression
def constExpr := condExpr

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

-- TODO: Add support for C declarations

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

declare_syntax_cat cStmt

-- jump statement
scoped syntax (name := gotoStmt) "goto" ident ";" : cStmt
scoped syntax (name := continueStmt) "continue" ";" : cStmt
scoped syntax (name := breakStmt) "break" ";" : cStmt
scoped syntax (name := returnStmt) "return" (ppSpace cExpr)? ";" : cStmt

-- compound statement
scoped syntax (name := compStmt) "{" cStmt* "}" : cStmt

-- expression statement
scoped syntax (name := exprStmt) cExpr ";" : cStmt

-- iteration statement
scoped syntax (name := whileStmt) "while " "(" cExpr ")" cStmt : cStmt
scoped syntax (name := doWhileStmt) "do " cStmt " while " "(" cExpr ")" : cStmt
scoped syntax (name := forStmt) "for " "(" «cExpr»? ";" «cExpr»? ";" «cExpr»? ")" cStmt : cStmt

-- selection statement
scoped syntax (name := ifStmt) "if " "(" cExpr ")" cStmt (" else " cStmt)? : cStmt
scoped syntax (name := switchStmt) "switch " "(" cExpr ")" cStmt : cStmt

-- labeled statement
scoped syntax (name := labelStmt) ident ": " cStmt : cStmt
scoped syntax (name := caseStmt) "case " constExpr ": " cStmt : cStmt
scoped syntax (name := defaultStmt) "default" ": " cStmt : cStmt
