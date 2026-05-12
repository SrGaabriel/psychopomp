namespace Psychopomp

inductive WrapMode where
  | none
  | hard (maxWidth : Nat)
  | truncate (margin : Nat)
  deriving Repr, BEq, Inhabited

inductive ColorMode where
  | always
  | never
  | auto
  deriving Repr, BEq, Inhabited

inductive GlyphSet where
  | unicode
  | ascii
  deriving Repr, BEq, Inhabited

structure RenderConfig where
  tabWidth : Nat := 4
  contextLines : Nat := 1
  wrap : WrapMode := .none
  colorMode : ColorMode := .auto
  glyphSet : GlyphSet := .unicode
  emitHandles : Bool := false
  defaultCausalDepth : Nat := 0
  maxStackedLabels : Nat := 0

def RenderConfig.colorsEnabled (cfg : RenderConfig) : Bool :=
  match cfg.colorMode with
  | .always => true
  | .never => false
  | .auto => true

end Psychopomp
