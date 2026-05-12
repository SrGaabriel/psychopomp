import Psychopomp.Core.Edit
import Psychopomp.Core.Span
import Psychopomp.Substrate.View

namespace Psychopomp.Render

private def replaceLines (lines : Array String) (range : Span) (newText : String) :
    Except String (Array String) := do
  let sLine := range.startLine
  let eLine := range.endLine
  if sLine == 0 ∨ sLine > lines.size then
    .error s!"applyEdits: startLine {sLine} out of range (substrate has {lines.size} lines)"
  if eLine == 0 ∨ eLine > lines.size then
    .error s!"applyEdits: endLine {eLine} out of range (substrate has {lines.size} lines)"
  let startLineRaw := lines[sLine - 1]!
  let endLineRaw := lines[eLine - 1]!
  let pre := (startLineRaw.take range.startCol).toString
  let suf := (endLineRaw.drop range.endCol).toString
  let combined : String := pre ++ newText ++ suf
  let newRows := (String.splitOn combined "\n").toArray
  let before := lines.extract 0 (sLine - 1)
  let after := lines.extract eLine lines.size
  return before ++ newRows ++ after

private partial def applyOne (lines : Array String) : Edit → Except String (Array String)
  | .replace _ range text => replaceLines lines range text
  | .insert _ at_ text =>
    let zeroWidth : Span :=
      { startLine := at_.startLine, startCol := at_.startCol
        endLine := at_.startLine, endCol := at_.startCol }
    replaceLines lines zeroWidth text
  | .delete _ range => replaceLines lines range ""
  | .seq edits => edits.foldlM applyOne lines

def applyEdits (view : SubstrateView) (edits : List Edit) : Except String SubstrateView := do
  let mut lines : Array String := Array.mkEmpty view.numLines
  for n in [1 : view.numLines + 1] do
    lines := lines.push (view.getLine n)
  let arr ← edits.foldlM applyOne lines
  return { view with
    numLines := arr.size
    getLine := fun n =>
      if n > 0 ∧ n - 1 < arr.size then arr[n - 1]!
      else "" }

end Psychopomp.Render
