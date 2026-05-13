import Kenosis
import Psychopomp.Core.Diagnostic
import Psychopomp.Substrate.Repository

namespace Psychopomp.Driver.Json

open Kenosis Psychopomp

structure SpanJson where
  startLine : Nat
  startCol : Nat
  endLine : Nat
  endCol : Nat
  deriving Serialize, Deserialize, BEq, Repr, Inhabited

structure ColorJson where
  kind : String
  name : Option String := none
  deriving Serialize, Deserialize, BEq, Repr, Inhabited

structure LabelStyleJson where
  pattern : String
  weight : Nat
  color : ColorJson
  tag : String
  linkGroup : Option String := none
  deriving Serialize, Deserialize, BEq, Repr, Inhabited

structure LabelJson where
  substrate : String
  range : SpanJson
  message : Option String := none
  style : LabelStyleJson
  deriving Serialize, Deserialize, BEq, Repr, Inhabited

structure SeverityJson where
  level : String
  phase : Option String := none
  certainty : String
  audiences : List String := []
  deriving Serialize, Deserialize, BEq, Repr, Inhabited

structure AttachmentJson where
  tag : String
  title : String
  body : List String
  deriving Serialize, Deserialize, BEq, Repr, Inhabited

structure EditJson where
  kind : String
  substrate : Option String := none
  range : Option SpanJson := none
  text : Option String := none
  edits : Option (List EditJson) := none
  deriving Serialize, Deserialize, BEq, Repr, Inhabited

structure QuickFixJson where
  description : String
  edits : List EditJson
  preview : Option String := none
  deriving Serialize, Deserialize, BEq, Repr, Inhabited

structure DiagJson where
  severity : SeverityJson
  code : Option String := none
  message : String
  primary : LabelJson
  secondary : List LabelJson := []
  notes : List String := []
  helps : List String := []
  attachments : List AttachmentJson := []
  fixes : List QuickFixJson := []
  causedBy : List DiagJson := []
  id : Option String := none
  deriving Serialize, Deserialize, BEq, Repr, Inhabited

private def levelStr : SeverityLevel → String
  | .error => "error"
  | .warning => "warning"
  | .info => "info"
  | .hint => "hint"
  | .lint => "lint"

private def certaintyStr : Certainty → String
  | .certain => "certain"
  | .suspected => "suspected"
  | .speculative => "speculative"

private def patternStr : UnderlinePattern → String
  | .solid => "solid"
  | .heavy => "heavy"
  | .dotted => "dotted"
  | .dashed => "dashed"
  | .wavy => "wavy"
  | .strikethrough => "strikethrough"
  | .doubleLine => "doubleLine"

private def colorRoleJson : ColorRole → ColorJson
  | .severity => { kind := "severity" }
  | .accent => { kind := "accent" }
  | .named n => { kind := "named", name := some n }
  | .none => { kind := "none" }

private def styleJson (s : LabelStyle) : LabelStyleJson :=
  { pattern := patternStr s.pattern
    weight := s.weight
    color := colorRoleJson s.color
    tag := s.tag
    linkGroup := s.linkGroup }

private def severityJson (s : Severity) : SeverityJson :=
  { level := levelStr s.level
    phase := s.phase
    certainty := certaintyStr s.certainty
    audiences := s.audiences }

private def spanJson (s : Span) : SpanJson :=
  { startLine := s.startLine, startCol := s.startCol
    endLine := s.endLine, endCol := s.endCol }

private def substrateName [SubstrateRepository R] (repo : R) (ref : SubstrateRef) : Except String String := do
  let view ← SubstrateRepository.get repo ref
  return view.name

private def labelJson [SubstrateRepository R] (repo : R) (l : Label) : Except String LabelJson := do
  let name ← substrateName repo l.substrate
  return { substrate := name
           range := spanJson l.range
           message := l.message
           style := styleJson l.style }

private partial def editJson [SubstrateRepository R]
    (repo : R) (e : Edit) : Except String EditJson := do
  match e with
  | .replace ref range newText =>
    let name ← substrateName repo ref
    return { kind := "replace", substrate := some name
             range := some (spanJson range), text := some newText }
  | .insert ref at_ text =>
    let name ← substrateName repo ref
    return { kind := "insert", substrate := some name
             range := some (spanJson at_), text := some text }
  | .delete ref range =>
    let name ← substrateName repo ref
    return { kind := "delete", substrate := some name
             range := some (spanJson range) }
  | .seq edits =>
    let children ← edits.mapM (editJson repo)
    return { kind := "seq", edits := some children }

private def quickFixJson [SubstrateRepository R]
    (repo : R) (qf : QuickFix) : Except String QuickFixJson := do
  let edits ← qf.edits.mapM (editJson repo)
  return { description := qf.description, edits, preview := qf.preview }

private def renderConfigForJson : RenderConfig :=
  { colorMode := .never, glyphSet := .unicode }

private def attachmentJson (a : Attachment) (sev : Severity) : AttachmentJson :=
  let rendered := a.render sev ViewState.empty renderConfigForJson
  { tag := a.tag, title := rendered.title, body := rendered.body }

partial def toJson [SubstrateRepository R]
    (d : Diagnostic) (repo : R) : Except String DiagJson := do
  let primary ← labelJson repo d.primary
  let secondary ← d.secondary.mapM (labelJson repo)
  let attachments := d.attachments.map (attachmentJson · d.severity)
  let fixes ← d.fixes.mapM (quickFixJson repo)
  let causedBy ← d.causedBy.mapM (fun child => toJson child repo)
  return { severity := severityJson d.severity
           code := d.code
           message := d.message
           primary, secondary, notes := d.notes, helps := d.helps
           attachments, fixes, causedBy
           id := d.id }

def encode [SubstrateRepository R] (d : Diagnostic) (repo : R) : Except String String := do
  let j ← toJson d repo
  return Kenosis.Json.encode j

def print [SubstrateRepository R] (d : Diagnostic) (repo : R) : IO Unit := do
  match encode d repo with
  | .ok s => IO.println s
  | .error e => IO.eprintln s!"diagnostic JSON encode error: {e}"

def eprint [SubstrateRepository R] (d : Diagnostic) (repo : R) : IO Unit := do
  match encode d repo with
  | .ok s => IO.eprintln s
  | .error e => IO.eprintln s!"diagnostic JSON encode error: {e}"

end Psychopomp.Driver.Json
