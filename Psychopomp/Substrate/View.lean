namespace Psychopomp

inductive SubstrateKind where
  | text
  | tree
  | graph
  | custom (name : String)
  deriving Repr, Hashable, BEq, Inhabited

abbrev SubstrateRef := Nat

structure SubstrateView where
  name : String
  kind : SubstrateKind := .text
  numLines : Nat
  getLine : Nat → String
  tabWidth : Nat := 4

instance : Inhabited SubstrateView where
  default := { name := "", numLines := 0, getLine := fun _ => "" }

end Psychopomp
