namespace Psychopomp

inductive SeverityLevel where
  | error
  | warning
  | info
  | hint
  | lint
  deriving Repr, Hashable, BEq, Inhabited

inductive Certainty where
  | certain
  | suspected
  | speculative
  deriving Repr, Hashable, BEq, Inhabited

structure Severity where
  level : SeverityLevel := .error
  phase : Option String := none
  certainty : Certainty := .certain
  audiences : List String := []
  deriving Repr, Hashable, BEq, Inhabited

namespace Severity

def error : Severity := { level := .error }
def warning : Severity := { level := .warning }
def info : Severity := { level := .info }
def hint : Severity := { level := .hint }
def lint : Severity := { level := .lint }

end Severity

end Psychopomp
