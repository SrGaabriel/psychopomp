import Psychopomp.Render.Fancy

namespace Psychopomp.Driver.Flush

open Psychopomp.Render

def render [SubstrateRepository R]
    (d : Diagnostic) (cfg : RenderConfig := {}) (repo : R) : Except String String := do
  let out ← Psychopomp.Render.renderDiagnostic d .empty cfg repo
  return out.text

def print [SubstrateRepository R]
    (d : Diagnostic) (cfg : RenderConfig := {}) (repo : R) : IO Unit := do
  match render d cfg repo with
  | .ok s => IO.println s
  | .error e => IO.eprintln s!"diagnostic render error: {e}"

def eprint [SubstrateRepository R]
    (d : Diagnostic) (cfg : RenderConfig := {}) (repo : R) : IO Unit := do
  match render d cfg repo with
  | .ok s => IO.eprintln s
  | .error e => IO.eprintln s!"diagnostic render error: {e}"

end Psychopomp.Driver.Flush
