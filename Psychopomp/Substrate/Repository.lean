import Std.Data.HashMap
import Psychopomp.Substrate.View

open Std

namespace Psychopomp

class SubstrateRepository (R : Type) where
  get : R → SubstrateRef → Except String SubstrateView
  put : R → SubstrateView → R × SubstrateRef

structure NaiveSubstrateRepository where
  views : Array SubstrateView := #[]
  byName : HashMap String SubstrateRef := ∅
  deriving Inhabited

instance : EmptyCollection NaiveSubstrateRepository := ⟨{}⟩

instance : SubstrateRepository NaiveSubstrateRepository where
  get repo ref :=
    if h : ref < repo.views.size then
      .ok repo.views[ref]
    else
      .error s!"SubstrateRepository.get: invalid ref {ref} (repository holds {repo.views.size} views)"
  put repo view :=
    match repo.byName[view.name]? with
    | some ref => (repo, ref)
    | none =>
      let ref := repo.views.size
      ({ views := repo.views.push view
         byName := repo.byName.insert view.name ref }, ref)

end Psychopomp
