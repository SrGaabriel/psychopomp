import Psychopomp.Core.Severity
import Psychopomp.Core.ViewState
import Psychopomp.Core.RenderConfig
import Psychopomp.Core.Json

namespace Psychopomp

structure RenderedAttachment where
  title : String
  body : List String
  deriving Inhabited

structure Attachment where
  tag : String
  render : Severity → ViewState → RenderConfig → RenderedAttachment
  payload : Option Json := none

instance : Inhabited Attachment where
  default := { tag := "", render := fun _ _ _ => default }

class AsAttachment (α : Type) where
  toAttachment : α → Attachment

instance : AsAttachment Attachment := ⟨id⟩

end Psychopomp
