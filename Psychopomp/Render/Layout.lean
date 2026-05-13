import Std.Data.HashMap
import Psychopomp.Core.Severity
import Psychopomp.Core.Span
import Psychopomp.Core.Label
import Psychopomp.Core.CharWidth
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
      let w := Char.visualWidth c
      if w > 0 then
        visualChars := visualChars.push c
        visualCol := visualCol + w
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

inductive ConnectorEntry where
  | multiSpan (spanIdx : Nat) (span : ResolvedLabel)
  | linkGroup (groupId : String) (members : Array ResolvedLabel)
  deriving Inhabited

namespace ConnectorEntry

def lineRange : ConnectorEntry → Nat × Nat
  | .multiSpan _ s => (s.range.startLine, s.range.endLine)
  | .linkGroup _ members => Id.run do
    let mut minL : Nat := 0
    let mut maxL : Nat := 0
    let mut first := true
    for m in members do
      if first then
        minL := m.range.startLine
        maxL := m.range.startLine
        first := false
      else
        minL := min minL m.range.startLine
        maxL := max maxL m.range.startLine
    return (minL, maxL)

end ConnectorEntry

def assignConnectors (entries : Array ConnectorEntry) : Array (Nat × ConnectorEntry) := Id.run do
  let sorted := (entries.toList.mergeSort fun a b =>
    let (aStart, aEnd) := a.lineRange
    let (bStart, bEnd) := b.lineRange
    if aStart ≠ bStart then aStart < bStart
    else aEnd ≥ bEnd).toArray
  let mut placed : Array (Nat × ConnectorEntry) := #[]
  for entry in sorted do
    let (s, e) := entry.lineRange
    let mut col : Nat := 0
    let mut found := false
    while !found do
      let clash := placed.any fun (c, other) =>
        let (os, oe) := other.lineRange
        c == col ∧ ¬ (e < os ∨ oe < s)
      if clash then col := col + 1
      else found := true
    placed := placed.push (col, entry)
  return placed

def collectLinkGroups (labels : Array ResolvedLabel) : Array (String × Array ResolvedLabel) := Id.run do
  let mut buckets : HashMap String (Array ResolvedLabel) := ∅
  let mut order : Array String := #[]
  for lbl in labels do
    match lbl.style.linkGroup with
    | none => pure ()
    | some g =>
      if !buckets.contains g then order := order.push g
      buckets := buckets.insert g ((buckets.getD g #[]).push lbl)
  let mut result : Array (String × Array ResolvedLabel) := #[]
  for g in order do
    let mems := buckets[g]!
    if mems.size ≥ 2 then
      let sorted := (mems.toList.mergeSort fun a b => a.range.startLine < b.range.startLine).toArray
      result := result.push (g, sorted)
  return result

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
