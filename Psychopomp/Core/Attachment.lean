import Psychopomp.Core.Severity
import Psychopomp.Core.ViewState
import Psychopomp.Core.RenderConfig

import Kenosis

open Kenosis.Json

namespace Psychopomp

structure RenderedAttachment where
  title : String
  body : List String
  deriving Inhabited

structure Attachment where
  tag : String
  render : Severity → ViewState → RenderConfig → RenderedAttachment
  payload : Option JsonValue := none

instance : Inhabited Attachment where
  default := { tag := "", render := fun _ _ _ => default }

class AsAttachment (α : Type) where
  toAttachment : α → Attachment

instance : AsAttachment Attachment := ⟨id⟩

end Psychopomp
