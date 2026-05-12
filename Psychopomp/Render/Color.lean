import Psychopomp.Core.Severity
import Psychopomp.Core.Label
import Psychopomp.Core.RenderConfig

namespace Psychopomp.Render

inductive Style where
  | severity
  | accent
  | gutter
  | dim
  | bold
  | none
  deriving Repr, Hashable, BEq, Inhabited

def Style.weight : Style → Nat
  | .severity => 100
  | .accent => 80
  | .gutter => 20
  | .bold => 10
  | .dim => 10
  | .none => 0

def Style.maxSeverity (a b : Style) : Style :=
  if a.weight ≥ b.weight then a else b

def Style.ofColorRole : ColorRole → Style
  | .severity => .severity
  | .accent => .accent
  | .named _ => .accent
  | .none => .none

namespace Ansi

def reset : String := "\x1b[0m"
def bold : String := "\x1b[1m"
def dim : String := "\x1b[2m"

def red : String := "\x1b[31m"
def green : String := "\x1b[32m"
def yellow : String := "\x1b[33m"
def blue : String := "\x1b[34m"
def magenta : String := "\x1b[35m"
def cyan : String := "\x1b[36m"
def white : String := "\x1b[37m"

def brightRed : String := "\x1b[91m"
def brightBlue : String := "\x1b[94m"
def brightCyan : String := "\x1b[96m"

end Ansi

def severityAnsi : SeverityLevel → String
  | .error => Ansi.bold ++ Ansi.red
  | .warning => Ansi.bold ++ Ansi.yellow
  | .info => Ansi.bold ++ Ansi.blue
  | .hint => Ansi.bold ++ Ansi.green
  | .lint => Ansi.bold ++ Ansi.cyan

def styleAnsi (style : Style) (severity : Severity) : String :=
  match style with
  | .severity => severityAnsi severity.level
  | .accent => Ansi.bold ++ Ansi.blue
  | .gutter => Ansi.brightBlue
  | .dim => Ansi.dim
  | .bold => Ansi.bold
  | .none => ""

def styleAnsiIn (cfg : RenderConfig) (style : Style) (severity : Severity) : String :=
  if cfg.colorsEnabled then styleAnsi style severity else ""

def resetIn (cfg : RenderConfig) : String :=
  if cfg.colorsEnabled then Ansi.reset else ""

end Psychopomp.Render
