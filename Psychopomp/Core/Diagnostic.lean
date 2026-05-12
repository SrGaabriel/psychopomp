import Psychopomp.Core.Severity
import Psychopomp.Core.Label
import Psychopomp.Core.Attachment
import Psychopomp.Core.Edit

namespace Psychopomp

inductive HandleKind where
  | expand
  | selectFix
  | witness
  deriving Repr, Hashable, BEq, Inhabited

structure Handle where
  id : String
  region : Nat × Nat
  kind : HandleKind
  deriving Repr, Inhabited

structure RenderedOutput where
  text : String
  handles : List Handle := []
  deriving Inhabited

structure Diagnostic where
  severity : Severity
  code : Option String := none
  message : String
  primary : Label
  secondary : List Label := []
  notes : List String := []
  helps : List String := []
  attachments : List Attachment := []
  fixes : List QuickFix := []
  causedBy : List Diagnostic := []
  id : Option String := none

instance : Inhabited Diagnostic where
  default := {
    severity := default
    message := ""
    primary := default
  }

end Psychopomp
