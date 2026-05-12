namespace Psychopomp.Render

structure Strokes where
  up : Bool := false
  down : Bool := false
  left : Bool := false
  right : Bool := false
  deriving Repr, BEq, Hashable, Inhabited

def Strokes.merge (a b : Strokes) : Strokes where
  up := a.up || b.up
  down := a.down || b.down
  left := a.left || b.left
  right := a.right || b.right

def Strokes.isEmpty (s : Strokes) : Bool :=
  !s.up && !s.down && !s.left && !s.right

def Strokes.toChar (s : Strokes) : Char :=
  match s.up, s.down, s.left, s.right with
  | false, false, false, false => ' '
  | true,  false, false, false => '│'
  | false, true,  false, false => '│'
  | true,  true,  false, false => '│'
  | false, false, true,  false => '─'
  | false, false, false, true  => '─'
  | false, false, true,  true  => '─'
  | true,  false, true,  false => '╯'
  | true,  false, false, true  => '╰'
  | false, true,  true,  false => '╮'
  | false, true,  false, true  => '╭'
  | true,  true,  true,  false => '┤'
  | true,  true,  false, true  => '├'
  | true,  false, true,  true  => '┴'
  | false, true,  true,  true  => '┬'
  | true,  true,  true,  true  => '┼'

namespace Strokes

def vertical : Strokes := { up := true, down := true }
def horizontal : Strokes := { left := true, right := true }
def cornerTopLeft : Strokes := { down := true, right := true }
def cornerTopRight : Strokes := { down := true, left := true }
def cornerBottomLeft : Strokes := { up := true, right := true }
def cornerBottomRight : Strokes := { up := true, left := true }
def teeRight : Strokes := { up := true, down := true, right := true }
def teeLeft : Strokes := { up := true, down := true, left := true }
def teeDown : Strokes := { down := true, left := true, right := true }
def teeUp : Strokes := { up := true, left := true, right := true }

end Strokes

namespace Glyph

def pipeDotted : Char := '┆'
def underlineCaret : Char := '^'
def underlineTilde : Char := '~'
def arrowRight : Char := '→'

end Glyph

end Psychopomp.Render
