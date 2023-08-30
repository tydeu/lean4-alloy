import Alloy.Util.OpaqueType

open Alloy

/-- A test docstring. -/
private opaque_type A

#print A
#print A.nonempty
#print A.nonemptyType
example : Nonempty A := inferInstance

@[inherit_doc A] opaque_type U.{u} : Type u

#print U
#print U.nonempty
#print U.nonemptyType
example : Nonempty U := inferInstance

opaque_type F (a : α) (b : β)

#print F
#print F.nonempty
#print F.nonemptyType
example : Nonempty (F a b)  := inferInstance

variable (n : Nat) in
opaque_type V (m := n) : Type

#print V
#print V.nonempty
#print V.nonemptyType
example : Nonempty (V n) := inferInstance

unsafe opaque_type S

#print S
#print S.nonempty
#print S.nonemptyType
unsafe example : Nonempty S := inferInstance
