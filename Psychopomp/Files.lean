import Std.Data.HashMap
import Psychopomp.Source

open Std

abbrev FileId := Nat

class FilesRepository (α : Type) where
  get : α → FileId → Except String Source
  put : α → Source → α × FileId

structure NaiveFilesRepository where
  files : Array Source := #[]
  byName : HashMap String FileId := ∅
  deriving Repr, BEq, Inhabited

namespace NaiveFilesRepository

instance : EmptyCollection NaiveFilesRepository := ⟨{}⟩

instance : FilesRepository NaiveFilesRepository where
  get repo id :=
    if h : id < repo.files.size then
      .ok repo.files[id]
    else
      .error s!"FilesRepository.get: invalid file id {id} (repository holds {repo.files.size} files)"
  put repo source :=
    match repo.byName[source.name]? with
    | some id => (repo, id)
    | none =>
      let id := repo.files.size
      let repo : NaiveFilesRepository :=
        { files := repo.files.push source
          byName := repo.byName.insert source.name id }
      (repo, id)

def NaiveFilesRepository.loadFile (repo : NaiveFilesRepository) (path : System.FilePath) :
    IO (NaiveFilesRepository × FileId) := do
  let name := path.toString
  if let some id := repo.byName[name]? then
    return (repo, id)
  let contents ← IO.FS.readFile path
  return FilesRepository.put repo { name, contents }

end NaiveFilesRepository
