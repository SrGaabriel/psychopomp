import Psychopomp.Source

abbrev FileId := Int

inductive Severity where
  | error
  | warning
  | lint
  deriving Repr, Hashable, BEq

structure Label where
  fileId : FileId
  range : Span
  message : Option String
  deriving Repr, Hashable, BEq

structure Diagnostic where
  severity : Severity
  code : Option String
  message : String
  primary : Label
  secondary : List Label
  notes : List String
  helps : List String
