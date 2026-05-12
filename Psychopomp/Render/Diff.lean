import Psychopomp.Core.Severity
import Psychopomp.Core.RenderConfig
import Psychopomp.Render.Color

namespace Psychopomp.Render

private def isDelim (c : Char) : Bool :=
  c == ' ' ∨ c == '\t' ∨ c == '(' ∨ c == ')' ∨ c == ',' ∨
  c == '[' ∨ c == ']' ∨ c == ':' ∨ c == ';' ∨ c == '→' ∨ c == '·'

def tokenize (s : String) : List String := Id.run do
  let mut tokens : Array String := #[]
  let mut cur : Array Char := #[]
  for c in s.toList do
    if isDelim c then
      if !cur.isEmpty then
        tokens := tokens.push (String.ofList cur.toList)
        cur := #[]
      tokens := tokens.push (String.singleton c)
    else
      cur := cur.push c
  if !cur.isEmpty then
    tokens := tokens.push (String.ofList cur.toList)
  return tokens.toList

private def commonPrefixLen : List String → List String → Nat
  | x :: xs, y :: ys => if x == y then 1 + commonPrefixLen xs ys else 0
  | _, _ => 0

private def commonSuffixLen (a b : List String) : Nat :=
  commonPrefixLen a.reverse b.reverse

def wordDiff (a b : String) (severity : Severity) (cfg : RenderConfig) : String × String :=
  let ta := tokenize a
  let tb := tokenize b
  let cp := commonPrefixLen ta tb
  let midA := ta.drop cp
  let midB := tb.drop cp
  let cs := min (commonSuffixLen midA midB) (min midA.length midB.length)
  let aMidLen := midA.length - cs
  let bMidLen := midB.length - cs
  let pre := String.join (ta.take cp)
  let aMid := String.join (midA.take aMidLen)
  let bMid := String.join (midB.take bMidLen)
  let suf := String.join (ta.drop (cp + aMidLen))
  let dim s :=
    if s.isEmpty ∨ !cfg.colorsEnabled then s
    else Ansi.dim ++ s ++ Ansi.reset
  let hi s :=
    if s.isEmpty ∨ !cfg.colorsEnabled then s
    else styleAnsi .severity severity ++ s ++ Ansi.reset
  let aOut := dim pre ++ hi aMid ++ dim suf
  let bOut := dim pre ++ hi bMid ++ dim suf
  (aOut, bOut)

end Psychopomp.Render
