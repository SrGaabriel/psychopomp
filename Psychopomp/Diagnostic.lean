import Psychopomp.Source
import Psychopomp.Files

inductive Severity where
  | error
  | warning
  | lint
  deriving Repr, Hashable, BEq, Inhabited

structure Label where
  fileId : FileId
  range : Span
  message : Option String
  deriving Repr, Hashable, BEq, Inhabited

structure RenderedAttachment where
  title : String
  body : List String
  deriving Inhabited

structure Attachment where
  tag : String
  render : Severity → RenderedAttachment

instance : Inhabited Attachment where
  default := { tag := "", render := fun _ => default }

class AsAttachment (α : Type) where
  toAttachment : α → Attachment

instance : AsAttachment Attachment := ⟨id⟩

structure Diagnostic where
  severity : Severity
  code : Option String
  message : String
  primary : Label
  secondary : List Label := []
  notes : List String := []
  helps : List String := []
  attachments : List Attachment := []
