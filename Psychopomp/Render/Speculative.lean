import Psychopomp.Core.Edit
import Psychopomp.Core.Span
import Psychopomp.Core.CharWidth
import Psychopomp.Substrate.View

namespace Psychopomp.Render


def splitAtVisualCol (raw : String) (vc : Nat) (tabWidth : Nat) : String × String := Id.run do
  let mut pre : Array Char := #[]
  let mut suf : Array Char := #[]
  let mut visualCol : Nat := 0
  let mut afterSplit : Bool := false
  let mut lastInSuf : Bool := false
  for c in raw.toList do
    let advance :=
      if c == '\t' then tabWidth - visualCol % tabWidth
      else Char.visualWidth c
    if advance == 0 then
      if lastInSuf then suf := suf.push c else pre := pre.push c
    else if afterSplit then
      suf := suf.push c
      lastInSuf := true
    else if visualCol + advance > vc then
      afterSplit := true
      suf := suf.push c
      lastInSuf := true
    else
      pre := pre.push c
      visualCol := visualCol + advance
      lastInSuf := false
      if visualCol == vc then afterSplit := true
  return (String.ofList pre.toList, String.ofList suf.toList)

private def replaceLines (tabWidth : Nat) (lines : Array String) (range : Span) (newText : String) :
    Except String (Array String) := do
  let sLine := range.startLine
  let eLine := range.endLine
  if sLine == 0 ∨ sLine > lines.size then
    .error s!"applyEdits: startLine {sLine} out of range (substrate has {lines.size} lines)"
  if eLine == 0 ∨ eLine > lines.size then
    .error s!"applyEdits: endLine {eLine} out of range (substrate has {lines.size} lines)"
  let startLineRaw := lines[sLine - 1]!
  let endLineRaw := lines[eLine - 1]!
  let (pre, _) := splitAtVisualCol startLineRaw range.startCol tabWidth
  let (_, suf) := splitAtVisualCol endLineRaw range.endCol tabWidth
  let combined : String := pre ++ newText ++ suf
  let newRows := (String.splitOn combined "\n").toArray
  let before := lines.extract 0 (sLine - 1)
  let after := lines.extract eLine lines.size
  return before ++ newRows ++ after

private partial def applyOne (tabWidth : Nat) (lines : Array String) : Edit → Except String (Array String)
  | .replace _ range text => replaceLines tabWidth lines range text
  | .insert _ at_ text =>
    let zeroWidth : Span :=
      { startLine := at_.startLine, startCol := at_.startCol
        endLine := at_.startLine, endCol := at_.startCol }
    replaceLines tabWidth lines zeroWidth text
  | .delete _ range => replaceLines tabWidth lines range ""
  | .seq edits => edits.foldlM (applyOne tabWidth) lines

def applyEdits (view : SubstrateView) (edits : List Edit) : Except String SubstrateView := do
  let mut lines : Array String := Array.mkEmpty view.numLines
  for n in [1 : view.numLines + 1] do
    lines := lines.push (view.getLine n)
  let arr ← edits.foldlM (applyOne view.tabWidth) lines
  return { view with
    numLines := arr.size
    getLine := fun n =>
      if n > 0 ∧ n - 1 < arr.size then arr[n - 1]!
      else "" }

end Psychopomp.Render
