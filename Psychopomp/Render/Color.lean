import Psychopomp.Diagnostic

namespace Psychopomp.Render

inductive Style where
  | primary
  | secondary
  | gutter
  | dim
  | bold
  | none
  deriving Repr, Hashable, BEq, Inhabited

namespace Ansi

def reset : String := "\x1b[0m"
def bold : String := "\x1b[1m"
def dim : String := "\x1b[2m"

def red : String := "\x1b[31m"
def green : String := "\x1b[32m"
def yellow : String := "\x1b[33m"
def blue : String := "\x1b[34m"
def cyan : String := "\x1b[36m"
def brightBlue : String := "\x1b[94m"

end Ansi

def severityAnsi : Severity → String
  | .error => Ansi.bold ++ Ansi.red
  | .warning => Ansi.bold ++ Ansi.yellow
  | .lint => Ansi.bold ++ Ansi.cyan

def styleAnsi (style : Style) (severity : Severity) : String :=
  match style with
  | .primary => severityAnsi severity
  | .secondary => Ansi.bold ++ Ansi.blue
  | .gutter => Ansi.brightBlue
  | .dim => Ansi.dim
  | .bold => Ansi.bold
  | .none => ""

end Psychopomp.Render
