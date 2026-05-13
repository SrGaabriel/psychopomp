namespace Psychopomp

inductive Json where
  | null
  | bool (b : Bool)
  | num (n : Float)
  | str (s : String)
  | arr (items : List Json)
  | obj (fields : List (String × Json))
  deriving Repr, BEq, Inhabited

namespace Json

def ofBool (b : Bool) : Json := .bool b
def ofNat (n : Nat) : Json := .num n.toFloat
def ofString (s : String) : Json := .str s
def ofStringList (xs : List String) : Json := .arr (xs.map .str)

end Json

end Psychopomp
