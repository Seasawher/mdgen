open System

/-- A new list created by removing the matching parts of two lists from the beginning. -/
def List.diff (as : List String) (bs : List String) : List String :=
  match as, bs with
  | [], [] => []
  | a, [] => a
  | [], _ => []
  | a :: as, b :: bs => if a == b then List.diff as bs else a :: as

namespace System.FilePath

/-- generate a filepath from path components -/
def ofComponents (l : List String) : FilePath :=
  l |> List.map (· ++ FilePath.pathSeparator.toString)
    |> List.foldl (· ++ ·) ""
    |> (String.dropRight · 1)
    |> FilePath.mk

/-- a function which returns an output file path component
given components of input and output directories. -/
def outputFilePath (inputDir : List String) (outputDir : List String)
    (path : List String) : List String :=
  let relativePath := path.diff inputDir
  outputDir ++ relativePath
    |> List.map (String.replace · ".lean" ".md")
    |> List.filter (· ≠ ".")

/-- Recursively outputs a list of the paths of lean files contained
in a directory whose path is `fp`. -/
partial def getLeanFilePaths (fp : FilePath) (acc : Array FilePath := #[]) :
    IO $ Array FilePath := do
  if ← fp.isDir then
    (← fp.readDir).foldlM (fun acc dir => getLeanFilePaths dir.path acc) acc
  else return if fp.extension == some "lean" then acc.push fp else acc

/-- create a file with given path and content. -/
def createFile (path : FilePath) (content : String) : IO Unit := do
  match path.parent with
  | none => IO.FS.writeFile path content
  | some parent =>
    IO.FS.createDirAll parent
    IO.FS.writeFile path content

end System.FilePath
