namespace Psychopomp

structure ByteRange where
  start : Nat
  stop : Nat
  deriving Repr, Hashable, BEq, Inhabited

structure Source where
  name : String
  contents : String
  deriving Repr, Hashable, BEq, Inhabited

end Psychopomp
