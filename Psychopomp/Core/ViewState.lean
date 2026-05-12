import Std.Data.HashMap

open Std

namespace Psychopomp

structure StreamStatus where
  phase : String
  progress : Option (Nat × Nat) := none
  deriving Repr, Inhabited

structure ViewState where
  expansions : HashMap String Bool := ∅
  selectedFix : HashMap String Nat := ∅
  witnesses : HashMap String String := ∅
  streamStatus : Option StreamStatus := none
  deriving Inhabited

def ViewState.empty : ViewState := default

end Psychopomp
