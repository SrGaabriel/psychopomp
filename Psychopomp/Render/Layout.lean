import Std.Data.HashMap
import Psychopomp.Core.Severity
import Psychopomp.Core.Span
import Psychopomp.Core.Label
import Psychopomp.Core.RenderConfig
import Psychopomp.Substrate.View

open Std

namespace Psychopomp.Render

structure LineData where
  lineNum : Nat
  raw : String
  visual : String
  visualWidth : Nat
  deriving Inhabited

namespace LineData

def build (lineNum : Nat) (raw : String) (tabWidth : Nat) : LineData := Id.run do
  let mut visualChars : Array Char := Array.mkEmpty raw.length
  let mut visualCol : Nat := 0
  for c in raw.toList do
    if c == '\t' then
      let advance := tabWidth - (visualCol % tabWidth)
      for _ in [0:advance] do visualChars := visualChars.push ' '
      visualCol := visualCol + advance
    else
      visualChars := visualChars.push c
      visualCol := visualCol + 1
  return { lineNum, raw, visual := String.ofList visualChars.toList, visualWidth := visualCol }

end LineData

structure ResolvedLabel where
  ref : SubstrateRef
  range : Span
  message : Option String
  style : LabelStyle
  deriving Repr, Hashable, BEq, Inhabited

namespace ResolvedLabel

def isMultiLine (l : ResolvedLabel) : Bool := !l.range.isSingleLine

end ResolvedLabel

structure SnippetGroup where
  ref : SubstrateRef
  view : SubstrateView
  labels : Array ResolvedLabel
  headerLabel : ResolvedLabel
  deriving Inhabited

def assignConnectors (spans : Array ResolvedLabel) : Array (Nat × ResolvedLabel) := Id.run do
  let sorted := (spans.toList.mergeSort fun a b =>
    if a.range.startLine ≠ b.range.startLine then a.range.startLine < b.range.startLine
    else a.range.endLine ≥ b.range.endLine).toArray
  let mut placed : Array (Nat × ResolvedLabel) := #[]
  for span in sorted do
    let mut col : Nat := 0
    let mut found := false
    while !found do
      let clash := placed.any fun (c, other) =>
        c == col ∧ ¬ (span.range.endLine < other.range.startLine ∨ other.range.endLine < span.range.startLine)
      if clash then col := col + 1
      else found := true
    placed := placed.push (col, span)
  return placed

def collectSnippetLines (group : SnippetGroup) (contextLines : Nat) : Array Nat := Id.run do
  let mut needed : Array Nat := #[]
  for lbl in group.labels do
    let lo := if lbl.range.startLine > contextLines then lbl.range.startLine - contextLines else 1
    let hi := min (lbl.range.endLine + contextLines) group.view.numLines
    for n in [lo : hi + 1] do
      needed := needed.push n
  needed.toList.eraseDups.mergeSort (· < ·) |>.toArray

def buildLineDataMap (view : SubstrateView) (lineNums : Array Nat) : HashMap Nat LineData := Id.run do
  let mut m : HashMap Nat LineData := ∅
  for n in lineNums do
    if !m.contains n then
      m := m.insert n (LineData.build n (view.getLine n) view.tabWidth)
  return m

end Psychopomp.Render
