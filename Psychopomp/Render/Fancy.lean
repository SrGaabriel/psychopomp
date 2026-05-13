import Std.Data.HashMap
import Psychopomp.Core.Diagnostic
import Psychopomp.Core.CharWidth
import Psychopomp.Substrate.Repository
import Psychopomp.Render.Color
import Psychopomp.Render.Glyph
import Psychopomp.Render.Cell
import Psychopomp.Render.Layout

open Std

namespace Psychopomp.Render

private def resolveLabel (l : Label) : ResolvedLabel :=
  { ref := l.substrate, range := l.range, message := l.message, style := l.style }

private def groupByRef
    [SubstrateRepository R] (repo : R) (d : Diagnostic) : Except String (List SnippetGroup) := do
  let primary := resolveLabel d.primary
  let secondaries := d.secondary.map resolveLabel
  let all : Array ResolvedLabel := #[primary] ++ secondaries.toArray
  let mut order : Array SubstrateRef := #[]
  let mut groups : HashMap SubstrateRef (Array ResolvedLabel) := ∅
  let mut headerForRef : HashMap SubstrateRef ResolvedLabel := ∅
  for l in all do
    if !groups.contains l.ref then
      order := order.push l.ref
      headerForRef := headerForRef.insert l.ref l
    groups := groups.insert l.ref ((groups.getD l.ref #[]).push l)
  let mut result : List SnippetGroup := []
  for ref in order do
    let view ← SubstrateRepository.get repo ref
    let labels := groups[ref]!
    let header := headerForRef[ref]!
    result := result ++ [{ ref, view, labels, headerLabel := header }]
  return result

private inductive RowKind where
  | content (lineNum : Nat)
  | skip (lineAbove lineBelow : Nat)
  | internal
  deriving Repr, Inhabited

private structure RowLayout where
  rowKind : Array RowKind
  contentRow : HashMap Nat Nat
  spanEndCornerRow : HashMap Nat Nat
  deriving Inhabited

private def RowLayout.totalRows (rl : RowLayout) : Nat := rl.rowKind.size

private def computeRowLayout
    (singles : Array ResolvedLabel) (multiSpans : Array ResolvedLabel)
    (snippetLines : Array Nat) : RowLayout := Id.run do
  let mut kinds : Array RowKind := #[]
  let mut contentRow : HashMap Nat Nat := ∅
  let mut spanEndCornerRow : HashMap Nat Nat := ∅
  let mut prevLine : Option Nat := none

  for ln in snippetLines do
    match prevLine with
    | some prev =>
      if ln > prev + 1 then kinds := kinds.push (.skip prev ln)
    | none => pure ()
    prevLine := some ln

    contentRow := contentRow.insert ln kinds.size
    kinds := kinds.push (.content ln)

    let lineSingles := singles.filter fun l => !l.isMultiLine ∧ l.range.startLine == ln
    if lineSingles.size > 0 then
      kinds := kinds.push .internal
      if lineSingles.size > 1 then
        let withMsg := lineSingles.filter (·.message.isSome)
        for _ in [0 : withMsg.size] do kinds := kinds.push .internal

    for i in [0 : multiSpans.size] do
      let ms := multiSpans[i]!
      if ms.range.endLine == ln then
        spanEndCornerRow := spanEndCornerRow.insert i kinds.size
        kinds := kinds.push .internal

  return { rowKind := kinds, contentRow, spanEndCornerRow }

private structure ColLayout where
  gutterWidth : Nat
  numConnectors : Nat
  deriving Repr, Inhabited

private def ColLayout.gutterPipeCol (cl : ColLayout) : Nat := cl.gutterWidth + 1
private def ColLayout.connectorCol (cl : ColLayout) (idx : Nat) : Nat := cl.gutterWidth + 2 + idx
private def ColLayout.contentStartCol (cl : ColLayout) : Nat := cl.gutterWidth + 3 + cl.numConnectors
private def ColLayout.visualToGridCol (cl : ColLayout) (vc : Nat) : Nat := cl.contentStartCol + vc

private def gutterWidthFor (snippetLines : Array Nat) : Nat :=
  max 1 (toString (snippetLines.foldl max 0)).length

private def writeGutter (g : Grid) (cl : ColLayout) (gs : GlyphSet)
    (row : Nat) (lineNum : Option Nat) : Grid := Id.run do
  let mut g := g
  match lineNum with
  | some n =>
    let s := toString n
    let pad := cl.gutterWidth - s.length
    let mut c := pad
    for ch in s.toList do
      g := g.writeChar row c ch .gutter .gutter
      c := c + 1
  | none => pure ()
  g := g.writeChar row cl.gutterPipeCol (Glyph.gutterPipe gs) .gutter .gutter
  return g

private def writeSkipGutter (g : Grid) (cl : ColLayout) (gs : GlyphSet) (row : Nat) : Grid :=
  g.writeChar row cl.gutterPipeCol (Glyph.pipeDotted gs) .gutter .gutter

private def writeContent (g : Grid) (cl : ColLayout) (row : Nat) (ld : LineData) : Grid := Id.run do
  let mut g := g
  let mut col := cl.contentStartCol
  for ch in ld.visual.toList do
    g := g.writeChar row col ch .none .content
    let w := Char.visualWidth ch
    if w == 2 then
      g := g.write row (col + 1) { content := .continuation, style := .none, layer := .content }
      col := col + 2
    else
      col := col + 1
  return g

private def labelStyle (l : ResolvedLabel) : Style := Style.ofColorRole l.style.color

private inductive UnderlineGlyph where
  | strokes
  | char (c : Char)
  | strike
  deriving Repr, Inhabited

private def underlineGlyph (p : UnderlinePattern) (gs : GlyphSet) : UnderlineGlyph :=
  match p with
  | .solid => .strokes
  | .heavy => .char (Glyph.underlineHeavy gs)
  | .wavy => .char (Glyph.underlineWavy gs)
  | .strikethrough => .strike
  | .dotted => .char (Glyph.underlineDotted gs)
  | .dashed => .char (Glyph.underlineDashed gs)
  | .doubleLine => .char (Glyph.underlineDouble gs)

private def drawUnderlineRange (g : Grid) (row : Nat) (startCol endCol : Nat)
    (style : Style) (glyph : UnderlineGlyph) : Grid :=
  match glyph with
  | .strokes => g.hline row startCol endCol style .underline
  | .char c => Id.run do
      let mut g := g
      for c' in [startCol : endCol + 1] do
        g := g.writeChar row c' c style .underline
      return g
  | .strike => g

private def drawStrike (g : Grid) (cl : ColLayout) (row : Nat) (lbl : ResolvedLabel)
    (gs : GlyphSet) : Grid := Id.run do
  let mut g := g
  let startCol := cl.visualToGridCol lbl.range.startCol
  let endCol := cl.visualToGridCol (max lbl.range.endCol (lbl.range.startCol + 1))
  let style := labelStyle lbl
  for c in [startCol : endCol] do
    g := g.writeChar row c (Glyph.strikethrough gs) style .underline
  return g

private def drawSingleLabel
    (g : Grid) (cl : ColLayout) (gs : GlyphSet) (underlineRow : Nat) (lbl : ResolvedLabel) : Grid := Id.run do
  let mut g := g
  let style := labelStyle lbl
  let glyph := underlineGlyph lbl.style.pattern gs
  let startCol := cl.visualToGridCol lbl.range.startCol
  let endColExcl := cl.visualToGridCol (max lbl.range.endCol (lbl.range.startCol + 1))
  match glyph with
  | .strike =>
    g := drawStrike g cl (underlineRow - 1) lbl gs
  | _ =>
    g := drawUnderlineRange g underlineRow startCol (endColExcl - 1) style glyph
    if let some msg := lbl.message then
      g := g.writeString underlineRow (endColExcl + 1) msg style .message
  return g

private def drawSingleLabelStack
    (g : Grid) (cl : ColLayout) (gs : GlyphSet) (underlineRow : Nat)
    (labels : Array ResolvedLabel) : Grid := Id.run do
  let mut g := g
  for lbl in labels do
    let style := labelStyle lbl
    let glyph := underlineGlyph lbl.style.pattern gs
    let startCol := cl.visualToGridCol lbl.range.startCol
    let endColExcl := cl.visualToGridCol (max lbl.range.endCol (lbl.range.startCol + 1))
    match glyph with
    | .strike => g := drawStrike g cl (underlineRow - 1) lbl gs
    | _ => g := drawUnderlineRange g underlineRow startCol (endColExcl - 1) style glyph
  let withMsg := labels.filter (·.message.isSome)
  let sorted := (withMsg.toList.mergeSort fun a b => a.range.startCol < b.range.startCol).toArray
  let count := sorted.size
  for lbl in sorted do
    if lbl.style.pattern == .solid then
      let anchorCol := cl.visualToGridCol lbl.range.startCol
      g := g.writeStrokes underlineRow anchorCol { down := true } (labelStyle lbl) .underline
  for i in [0 : count] do
    let lbl := sorted[i]!
    let dropRow := underlineRow + 1 + (count - 1 - i)
    let anchorCol := cl.visualToGridCol lbl.range.startCol
    let style := labelStyle lbl
    for j in [0 : i] do
      let other := sorted[j]!
      let otherCol := cl.visualToGridCol other.range.startCol
      g := g.writeStrokes dropRow otherCol Strokes.vertical (labelStyle other) .connector
    g := g.writeStrokes dropRow anchorCol { up := true, right := true } style .connector
    g := g.hline dropRow (anchorCol + 1) (anchorCol + 2) style .underline
    if let some msg := lbl.message then
      g := g.writeString dropRow (anchorCol + 4) msg style .message
  return g

private def dominantStyle (labels : Array ResolvedLabel) : Style := Id.run do
  let mut best : Option ResolvedLabel := none
  for l in labels do
    match best with
    | none => best := some l
    | some b => if l.style.weight > b.style.weight then best := some l
  match best with
  | some l => Style.ofColorRole l.style.color
  | none => .accent

private def drawLinkGroup
    (g : Grid) (cl : ColLayout) (connectorIdx : Nat)
    (memberRows : Array Nat) (members : Array ResolvedLabel) : Grid := Id.run do
  let mut g := g
  if memberRows.size < 2 then return g
  let sorted := (memberRows.toList.mergeSort (· < ·)).toArray
  let minRow := sorted[0]!
  let maxRow := sorted[sorted.size - 1]!
  let cCol := cl.connectorCol connectorIdx
  let style := dominantStyle members
  if minRow + 1 ≤ maxRow - 1 then
    g := g.vline cCol (minRow + 1) (maxRow - 1) style .connector
  g := g.writeStrokes minRow cCol { down := true, right := true } style .connector
  g := g.writeStrokes maxRow cCol { up := true, right := true } style .connector
  for r in sorted do
    if r ≠ minRow ∧ r ≠ maxRow then
      g := g.writeStrokes r cCol Strokes.teeRight style .connector
  return g

private def drawMultiSpan
    (g : Grid) (cl : ColLayout) (startRow endRow connectorIdx : Nat)
    (span : ResolvedLabel) : Grid := Id.run do
  let mut g := g
  let cCol := cl.connectorCol connectorIdx
  let style := labelStyle span
  if startRow + 1 ≤ endRow - 1 then
    g := g.vline cCol (startRow + 1) (endRow - 1) style .connector
  g := g.writeStrokes startRow cCol { down := true, right := true } style .connector
  g := g.writeStrokes endRow cCol { up := true, right := true } style .connector
  let endVisualCol := max span.range.endCol 1
  let endGridCol := cl.visualToGridCol endVisualCol - 1
  let lo := cCol + 1
  let hi := max endGridCol lo
  g := g.hline endRow lo hi style .underline
  if let some msg := span.message then
    g := g.writeString endRow (hi + 2) msg style .message
  return g

private def renderSnippetBlock
    (group : SnippetGroup) (gutterWidth : Nat) (severity : Severity)
    (cfg : RenderConfig) : String := Id.run do
  let snippetLines := collectSnippetLines group cfg.contextLines
  let lineDataMap := buildLineDataMap group.view snippetLines

  let multiSpans : Array ResolvedLabel := group.labels.filter ResolvedLabel.isMultiLine
  let linkGroups := collectLinkGroups group.labels
  let mut entries : Array ConnectorEntry := #[]
  for i in [0 : multiSpans.size] do
    entries := entries.push (.multiSpan i multiSpans[i]!)
  for (gid, mems) in linkGroups do
    entries := entries.push (.linkGroup gid mems)
  let placed := assignConnectors entries
  let numConnectors := placed.foldl (fun acc (c, _) => max acc (c + 1)) 0
  let cl : ColLayout := { gutterWidth, numConnectors }
  let layout := computeRowLayout group.labels multiSpans snippetLines
  let total := layout.totalRows
  let gs := cfg.glyphSet
  let mut g : Grid := Grid.empty

  g := writeGutter g cl gs 0 none
  let off : Nat := 1

  for r in [0 : total] do
    let realRow := r + off
    match layout.rowKind[r]! with
    | .content ln =>
      g := writeGutter g cl gs realRow (some ln)
      g := writeContent g cl realRow lineDataMap[ln]!
    | .skip _ _ => g := writeSkipGutter g cl gs realRow
    | .internal => g := writeGutter g cl gs realRow none

  for ln in snippetLines do
    let contentR := layout.contentRow[ln]! + off
    let singles := group.labels.filter fun l => !l.isMultiLine ∧ l.range.startLine == ln
    if singles.size == 1 then
      g := drawSingleLabel g cl gs (contentR + 1) singles[0]!
    else if singles.size > 1 then
      let sorted := (singles.toList.mergeSort fun a b => a.range.startCol < b.range.startCol).toArray
      g := drawSingleLabelStack g cl gs (contentR + 1) sorted

  for (cIdx, entry) in placed do
    match entry with
    | .multiSpan spanIdx span =>
      let startR := layout.contentRow[span.range.startLine]! + off
      let endR := layout.spanEndCornerRow[spanIdx]! + off
      g := drawMultiSpan g cl startR endR cIdx span
    | .linkGroup _ members =>
      let memberRows : Array Nat := members.map fun m =>
        layout.contentRow.getD m.range.startLine 0 + off
      g := drawLinkGroup g cl cIdx memberRows members

  for r in [0 : total] do
    match layout.rowKind[r]! with
    | .skip above below =>
      let realRow := r + off
      for (cIdx, entry) in placed do
        let (eStart, eEnd) := entry.lineRange
        if eStart ≤ above ∧ eEnd ≥ below then
          let style : Style := match entry with
            | .multiSpan _ span => labelStyle span
            | .linkGroup _ members => dominantStyle members
          g := g.writeChar realRow (cl.connectorCol cIdx) (Glyph.pipeDotted gs) style .connector
    | _ => pure ()

  g := writeGutter g cl gs (total + off) none
  return g.flush severity cfg

private def severityWord : SeverityLevel → String
  | .error => "error"
  | .warning => "warning"
  | .info => "info"
  | .hint => "hint"
  | .lint => "lint"

private def renderHeader (d : Diagnostic) (cfg : RenderConfig) : String :=
  let sev := styleAnsiIn cfg .severity d.severity ++ severityWord d.severity.level
  let code := match d.code with | some c => "[" ++ c ++ "]" | none => ""
  let bold := if cfg.colorsEnabled then Ansi.bold else ""
  let reset := resetIn cfg
  sev ++ code ++ reset ++ ": " ++ bold ++ d.message ++ reset

private def renderLocationArrow
    (group : SnippetGroup) (gutterWidth : Nat) (cfg : RenderConfig) : String :=
  let pad := String.ofList (List.replicate gutterWidth ' ')
  let arrowGlyph := match cfg.glyphSet with
    | .unicode => "╭─" ++ String.singleton (Glyph.arrowRight .unicode)
    | .ascii => ",-" ++ String.singleton (Glyph.arrowRight .ascii)
  let arrow :=
    if cfg.colorsEnabled then Ansi.brightBlue ++ arrowGlyph ++ Ansi.reset
    else arrowGlyph
  let loc := group.view.name ++ ":" ++ toString group.headerLabel.range.startLine ++ ":" ++
             toString (group.headerLabel.range.startCol + 1)
  pad ++ " " ++ arrow ++ " " ++ loc

private def renderFooterLine
    (cfg : RenderConfig) (gutterWidth : Nat) (markerColor : String) (label : String) (text : String) : String :=
  let pad := String.ofList (List.replicate gutterWidth ' ')
  let markerC := if cfg.colorsEnabled then markerColor else ""
  let boldC := if cfg.colorsEnabled then Ansi.bold else ""
  let reset := resetIn cfg
  pad ++ " " ++ markerC ++ "=" ++ reset ++ " " ++ boldC ++ label ++ reset ++ ": " ++ text

private def renderAttachmentBlock
    (cfg : RenderConfig) (gutterWidth : Nat) (ra : RenderedAttachment) : List String :=
  let pad := String.ofList (List.replicate gutterWidth ' ')
  let markerC := if cfg.colorsEnabled then Ansi.green else ""
  let reset := resetIn cfg
  let titleLine := pad ++ " " ++ markerC ++ "=" ++ reset ++ " " ++ ra.title
  let bodyPrefix := pad ++ "   "
  titleLine :: ra.body.map (bodyPrefix ++ ·)

private def renderQuickFixListing
    (cfg : RenderConfig) (gutterWidth : Nat) (fixes : List QuickFix) : List String :=
  if fixes.isEmpty then []
  else
    let pad := String.ofList (List.replicate gutterWidth ' ')
    let markerC := if cfg.colorsEnabled then Ansi.green else ""
    let boldC := if cfg.colorsEnabled then Ansi.bold else ""
    let reset := resetIn cfg
    let title := pad ++ " " ++ markerC ++ "=" ++ reset ++ " " ++ boldC ++ "available fixes:" ++ reset
    let bodyPrefix := pad ++ "   "
    let lines := fixes.zipIdx.map fun (qf, i) =>
      bodyPrefix ++ "⌖ #" ++ toString (i + 1) ++ ": " ++ qf.description
    title :: lines

private def renderCausalSummary
    (cfg : RenderConfig) (gutterWidth : Nat) (n : Nat) : Option String :=
  if n == 0 then none
  else
    let pad := String.ofList (List.replicate gutterWidth ' ')
    let dim := if cfg.colorsEnabled then Ansi.dim else ""
    let reset := resetIn cfg
    let plural := if n == 1 then "diagnostic" else "diagnostics"
    some (pad ++ " " ++ dim ++ "⤷ " ++ toString n ++ " downstream " ++ plural ++
      " suppressed" ++ reset)

def renderDiagnostic [SubstrateRepository R]
    (d : Diagnostic) (view : ViewState := .empty) (cfg : RenderConfig := {}) (repo : R)
    : Except String RenderedOutput := do
  let _ := view 
  let groups ← groupByRef repo d

  let gutterWidth := groups.foldl (init := 1) fun acc g =>
    max acc (gutterWidthFor (collectSnippetLines g cfg.contextLines))

  let mut out : Array String := #[renderHeader d cfg]
  for g in groups do
    out := out.push (renderLocationArrow g gutterWidth cfg)
    out := out.push (renderSnippetBlock g gutterWidth d.severity cfg)

  for note in d.notes do
    out := out.push (renderFooterLine cfg gutterWidth
      (if cfg.colorsEnabled then Ansi.brightBlue else "") "note" note)
  for help in d.helps do
    out := out.push (renderFooterLine cfg gutterWidth
      (if cfg.colorsEnabled then Ansi.green else "") "help" help)
  for a in d.attachments do
    let ra := a.render d.severity view cfg
    for line in renderAttachmentBlock cfg gutterWidth ra do
      out := out.push line
  for line in renderQuickFixListing cfg gutterWidth d.fixes do
    out := out.push line
  match renderCausalSummary cfg gutterWidth d.causedBy.length with
  | some line => out := out.push line
  | none => pure ()

  return { text := String.intercalate "\n" out.toList, handles := [] }

end Psychopomp.Render
