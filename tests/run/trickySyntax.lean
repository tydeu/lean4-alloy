import Alloy.C.Grammar

open Lean Alloy.C

/- Syntax which is sensitive to elided semicolons. -/

#eval format <| Unhygienic.run `(cStmt|
  for (uint32_t i = 0; i < len_c; ++i);
)

#eval format <| Unhygienic.run `(cStmt|
  lean_to_closure(o)->m_objs[i] = a;
)

#eval format <| Unhygienic.run `(cStmtLike| int x)
#eval format <| Unhygienic.run `(cStmtLike| int x;)
#eval format <| Unhygienic.run `(declaration| struct foo)
#eval format <| Unhygienic.run `(declaration| struct foo;)
#eval format <| Unhygienic.run `(declaration| union foo)
#eval format <| Unhygienic.run `(declaration| union foo;)
#eval format <| Unhygienic.run `(declaration| enum foo)
#eval format <| Unhygienic.run `(declaration| enum foo;)

/- Ensure C grammar is not adding unexpected tokens. -/

#eval let goto : Nat := default; goto
#eval format <| Unhygienic.run `(cStmt| default: foo)
#eval format <| Unhygienic.run `(cStmt| goto default)
