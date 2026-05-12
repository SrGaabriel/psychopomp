import Std.Data.HashMap
import Psychopomp.Render.Color
import Psychopomp.Render.Glyph

open Std

namespace Psychopomp.Render

inductive Layer where
  | background
  | gutter
  | content
  | connector
  | underline
  | message
  deriving Repr, BEq, Hashable, Inhabited

def Layer.toNat : Layer → Nat
  | .background => 0
  | .gutter => 1
  | .content => 2
  | .connector => 3
  | .underline => 4
  | .message => 5

instance : LT Layer := ⟨fun a b => a.toNat < b.toNat⟩
instance : LE Layer := ⟨fun a b => a.toNat ≤ b.toNat⟩
instance (a b : Layer) : Decidable (a < b) := inferInstanceAs (Decidable (a.toNat < b.toNat))
instance (a b : Layer) : Decidable (a ≤ b) := inferInstanceAs (Decidable (a.toNat ≤ b.toNat))

inductive CellContent where
  | char (c : Char)
  | strokes (s : Strokes)
  deriving Repr, BEq, Hashable, Inhabited

def CellContent.render : CellContent → Char
  | .char c => c
  | .strokes s => s.toChar

structure Cell where
  content : CellContent
  style : Style
  layer : Layer
  deriving Repr, BEq, Hashable, Inhabited

def Cell.char : Cell := { content := .char ' ', style := .none, layer := .background }

structure Grid where
  cells : HashMap (Nat × Nat) Cell := ∅
  rows : Nat := 0
  cols : Nat := 0
  deriving Inhabited

namespace Grid

def empty : Grid := {}

def write (g : Grid) (row col : Nat) (cell : Cell) : Grid :=
  let merged :=
    match g.cells[(row, col)]? with
    | none => cell
    | some existing =>
      if cell.layer > existing.layer then cell
      else if cell.layer < existing.layer then existing
      else
        match existing.content, cell.content with
        | .strokes a, .strokes b =>
          { cell with content := .strokes (a.merge b) }
        | _, _ => cell
  { g with
    cells := g.cells.insert (row, col) merged
    rows := max g.rows (row + 1)
    cols := max g.cols (col + 1) }

def writeChar (g : Grid) (row col : Nat) (c : Char) (style : Style := .none) (layer : Layer := .content) : Grid :=
  g.write row col { content := .char c, style, layer }

def writeStrokes (g : Grid) (row col : Nat) (s : Strokes) (style : Style) (layer : Layer) : Grid :=
  g.write row col { content := .strokes s, style, layer }

def hline (g : Grid) (row col1 col2 : Nat) (style : Style) (layer : Layer := .underline) : Grid :=
  if col1 > col2 then g
  else Id.run do
    let mut g := g
    for c in [col1:col2+1] do
      let strokes :=
        if c == col1 then { Strokes.horizontal with left := false }
        else if c == col2 then { Strokes.horizontal with right := false }
        else Strokes.horizontal
      g := g.writeStrokes row c strokes style layer
    return g

def vline (g : Grid) (col row1 row2 : Nat) (style : Style) (layer : Layer := .connector) : Grid :=
  if row1 > row2 then g
  else Id.run do
    let mut g := g
    for r in [row1:row2+1] do
      let strokes :=
        if r == row1 then { Strokes.vertical with up := false }
        else if r == row2 then { Strokes.vertical with down := false }
        else Strokes.vertical
      g := g.writeStrokes r col strokes style layer
    return g

def writeString (g : Grid) (row col : Nat) (s : String) (style : Style := .none) (layer : Layer := .message) : Grid := Id.run do
  let mut g := g
  let mut c := col
  for ch in s.toList do
    g := g.writeChar row c ch style layer
    c := c + 1
  return g

def flush (g : Grid) (severity : Severity) : String := Id.run do
  let mut out : Array String := #[]
  for r in [0:g.rows] do
    let mut maxCol : Nat := 0
    let mut hasAny : Bool := false
    for c in [0:g.cols] do
      if g.cells.contains (r, c) then
        maxCol := c
        hasAny := true
    if !hasAny then
      out := out.push ""
    else
      let mut line : String := ""
      let mut curStyle : Style := .none
      for c in [0:maxCol+1] do
        let cell := g.cells[(r, c)]?.getD Cell.char
        if cell.style != curStyle then
          if curStyle != .none then
            line := line ++ Ansi.reset
          if cell.style != .none then
            line := line ++ styleAnsi cell.style severity
          curStyle := cell.style
        line := line.push cell.content.render
      if curStyle != .none then
        line := line ++ Ansi.reset
      out := out.push line
  return String.intercalate "\n" out.toList

end Grid

end Psychopomp.Render
