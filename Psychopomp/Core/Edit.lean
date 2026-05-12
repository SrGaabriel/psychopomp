import Psychopomp.Core.Span
import Psychopomp.Substrate.View

namespace Psychopomp

inductive Edit where
  | replace (substrate : SubstrateRef) (range : Span) (newText : String)
  | insert (substrate : SubstrateRef) («at» : Span) (text : String)
  | delete (substrate : SubstrateRef) (range : Span)
  | seq (edits : List Edit)
  deriving Repr, Hashable, BEq, Inhabited

structure QuickFix where
  description : String
  edits : List Edit
  preview : Option String := none
  deriving Repr, Hashable, BEq, Inhabited

end Psychopomp
