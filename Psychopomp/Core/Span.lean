namespace Psychopomp

structure Span where
  startLine : Nat := 1
  startCol : Nat := 0
  endLine : Nat := 1
  endCol : Nat := 0
  deriving Repr, Hashable, BEq, Inhabited

namespace Span

def isSingleLine (s : Span) : Bool := s.startLine == s.endLine
def isZeroWidth (s : Span) : Bool := s.startLine == s.endLine ∧ s.startCol == s.endCol

def lineCount (s : Span) : Nat :=
  if s.endLine ≥ s.startLine then s.endLine - s.startLine + 1 else 1

end Span

end Psychopomp
