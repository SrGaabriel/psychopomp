import Psychopomp.Core.Span
import Psychopomp.Substrate.View

namespace Psychopomp

inductive UnderlinePattern where
  | solid
  | heavy
  | dotted
  | dashed
  | wavy
  | strikethrough
  | doubleLine
  deriving Repr, Hashable, BEq, Inhabited

inductive ColorRole where
  | severity
  | accent
  | named (name : String)
  | none
  deriving Repr, Hashable, BEq, Inhabited

structure LabelStyle where
  pattern : UnderlinePattern := .solid
  weight : Nat := 50
  color : ColorRole := .severity
  tag : String := ""
  linkGroup : Option String := none
  deriving Repr, Hashable, BEq, Inhabited

namespace LabelStyle

def error : LabelStyle :=
  { pattern := .solid, weight := 100, color := .severity, tag := "error" }

def support : LabelStyle :=
  { pattern := .solid, weight := 50, color := .accent, tag := "support" }

end LabelStyle

structure Label where
  substrate : SubstrateRef
  range : Span
  message : Option String := none
  style : LabelStyle := .error
  deriving Repr, Hashable, BEq, Inhabited

end Psychopomp
