import Psychopomp.Source
import Psychopomp.Core.Span
import Psychopomp.Core.Label
import Psychopomp.Core.CharWidth
import Psychopomp.Substrate.View

namespace Psychopomp

structure LineIndex where
  lineStarts : Array Nat
  totalBytes : Nat
  deriving Inhabited

namespace LineIndex

def build (contents : String) : LineIndex := Id.run do
  let total := contents.utf8ByteSize
  let mut starts : Array Nat := #[0]
  let mut pos : String.Pos.Raw := ⟨0⟩
  while pos.byteIdx < total do
    let c := pos.get contents
    pos := pos.next contents
    if c == '\r' ∧ pos.byteIdx < total ∧ pos.get contents == '\n' then
      pos := pos.next contents
      if pos.byteIdx < total then starts := starts.push pos.byteIdx
    else if c == '\n' ∨ c == '\r' then
      if pos.byteIdx < total then starts := starts.push pos.byteIdx
  return { lineStarts := starts, totalBytes := total }

def numLines (idx : LineIndex) : Nat := idx.lineStarts.size

def lineOf (idx : LineIndex) (byteOffset : Nat) : Nat := Id.run do
  let bo := min byteOffset idx.totalBytes
  let mut lo : Nat := 0
  let mut hi : Nat := idx.lineStarts.size
  while lo + 1 < hi do
    let mid := (lo + hi) / 2
    if idx.lineStarts[mid]! ≤ bo then lo := mid
    else hi := mid
  return lo + 1

def lineByteRange (idx : LineIndex) (lineNum : Nat) : Nat × Nat :=
  if lineNum == 0 ∨ lineNum > idx.lineStarts.size then (0, 0)
  else
    let s := idx.lineStarts[lineNum - 1]!
    let e := if lineNum < idx.lineStarts.size then idx.lineStarts[lineNum]! else idx.totalBytes
    (s, e)

def getLine (idx : LineIndex) (contents : String) (lineNum : Nat) : String :=
  let (s, e) := idx.lineByteRange lineNum
  if s ≥ e then ""
  else
    let raw := String.Pos.Raw.extract contents ⟨s⟩ ⟨e⟩
    if raw.endsWith "\r\n" then (raw.dropEnd 2).toString
    else if raw.endsWith "\n" ∨ raw.endsWith "\r" then (raw.dropEnd 1).toString
    else raw

def locate (idx : LineIndex) (byteOffset : Nat) : Nat × Nat :=
  let ln := idx.lineOf byteOffset
  let (lineStart, _) := idx.lineByteRange ln
  (ln, byteOffset - lineStart)

end LineIndex

def byteColToVisualCol (raw : String) (byteCol : Nat) (tabWidth : Nat) : Nat := Id.run do
  let target := min byteCol raw.utf8ByteSize
  let mut pos : String.Pos.Raw := ⟨0⟩
  let mut visualCol : Nat := 0
  while pos.byteIdx < target do
    let c := pos.get raw
    pos := pos.next raw
    let charEnd := pos.byteIdx
    if charEnd > target then
      return visualCol
    if c == '\t' then
      visualCol := visualCol + (tabWidth - visualCol % tabWidth)
    else
      visualCol := visualCol + Char.visualWidth c
  return visualCol

structure SourceContext where
  source : Source
  lineIndex : LineIndex
  tabWidth : Nat := 4
  deriving Inhabited

namespace SourceContext

def of (src : Source) (tabWidth : Nat := 4) : SourceContext :=
  { source := src, lineIndex := LineIndex.build src.contents, tabWidth }

def toSubstrateView (ctx : SourceContext) : SubstrateView :=
  let idx := ctx.lineIndex
  let contents := ctx.source.contents
  { name := ctx.source.name
    kind := .text
    numLines := idx.numLines
    getLine := fun n => idx.getLine contents n
    tabWidth := ctx.tabWidth }

def spanOfBytes (ctx : SourceContext) (byteStart byteStop : Nat) : Span := Id.run do
  let (startLine, startByteCol) := ctx.lineIndex.locate byteStart
  let startRaw := ctx.lineIndex.getLine ctx.source.contents startLine
  let startCol := byteColToVisualCol startRaw startByteCol ctx.tabWidth

  let (rawEndLine, rawEndByteCol) := ctx.lineIndex.locate byteStop
  let (endLine, endRaw, endByteCol) :=
    if rawEndByteCol == 0 ∧ rawEndLine > startLine then
      let prev := rawEndLine - 1
      let prevRaw := ctx.lineIndex.getLine ctx.source.contents prev
      (prev, prevRaw, prevRaw.utf8ByteSize)
    else
      (rawEndLine, ctx.lineIndex.getLine ctx.source.contents rawEndLine, rawEndByteCol)
  let endCol := byteColToVisualCol endRaw endByteCol ctx.tabWidth

  return { startLine, startCol, endLine, endCol }

def label (ctx : SourceContext) (substrate : SubstrateRef)
    (byteStart byteStop : Nat) (message : Option String := none)
    (style : LabelStyle := .error) : Label :=
  { substrate, range := ctx.spanOfBytes byteStart byteStop, message, style }

end SourceContext

end Psychopomp
