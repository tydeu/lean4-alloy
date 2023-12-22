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
