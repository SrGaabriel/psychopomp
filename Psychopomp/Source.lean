structure Span where
  start : Nat
  stop : Nat
  deriving Repr, Hashable, BEq

structure Source where
  name : String
  contents : String
  deriving Repr, Hashable, BEq

structure SpanContent where
  fileName : String
  data : String
  line : Nat
  column : Nat
  firstLine : Nat
  lineCount : Nat
  deriving Repr, Hashable, BEq

partial def Source.getSpanContent (source : Source) (span : Span)
    (linesBefore : Nat := 0) (linesAfter : Nat := 0) : SpanContent := Id.run do
  let s := source.contents
  let total : Nat := s.utf8ByteSize
  let spanStart : Nat := min span.start total
  let spanEnd : Nat := min (max span.stop spanStart) total

  let mut ring : Array Nat := #[0]
  let mut line : Nat := 1
  let mut col : Nat := 1
  let mut pos : String.Pos.Raw := ⟨0⟩

  let mut anchored : Bool := false
  let mut spanLine : Nat := 1
  let mut spanColumn : Nat := 1
  let mut snippetStart : Nat := 0
  let mut snippetFirstLine : Nat := 1

  let mut spanLineClosed : Bool := false
  let mut afterCount : Nat := 0
  let mut snippetEnd : Nat := total
  let mut lastLineEnd : Nat := 0
  let mut done : Bool := false

  while pos.byteIdx < total do
    if done then break

    if !anchored && pos.byteIdx ≥ spanStart then
      anchored := true
      snippetStart := ring[0]!
      snippetFirstLine := line - (ring.size - 1)
      spanLine := line
      spanColumn := col

    let c := pos.get s
    let cStart := pos.byteIdx
    pos := pos.next s
    if c == '\r' && pos.byteIdx < total && pos.get s == '\n' then
      pos := pos.next s

    if c == '\n' || c == '\r' then
      let lineStart := pos.byteIdx
      if !anchored then
        ring := ring.push lineStart
        if ring.size > linesBefore + 1 then
          ring := ring.extract 1 ring.size
        line := line + 1
        col := 1
      else
        if !spanLineClosed then
          if cStart + 1 ≥ spanEnd then
            spanLineClosed := true
            lastLineEnd := cStart
            snippetEnd := lineStart
            if linesAfter == 0 then
              done := true
          else
            snippetEnd := lineStart
        else
          afterCount := afterCount + 1
          lastLineEnd := cStart
          snippetEnd := lineStart
          if afterCount ≥ linesAfter then
            done := true
        line := line + 1
        col := 1
    else
      col := col + 1

  if !anchored then
    snippetStart := ring[0]!
    snippetFirstLine := line - (ring.size - 1)
    spanLine := line
    spanColumn := col

  if !done then
    snippetEnd := pos.byteIdx
    lastLineEnd := snippetEnd

  let finalEnd := if done then lastLineEnd else snippetEnd
  let lastLine := if done then line - 1 else line
  return {
    fileName := source.name
    data := String.Pos.Raw.extract s ⟨snippetStart⟩ ⟨finalEnd⟩
    line := spanLine
    column := spanColumn
    firstLine := snippetFirstLine
    lineCount := lastLine - snippetFirstLine + 1
  }
