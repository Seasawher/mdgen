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
  l |>.intersperse FilePath.pathSeparator.toString
    |> List.foldl (· ++ ·) ""
    |> FilePath.mk

/-- a function which returns an output file path component
given components of input and output directories. -/
def outputFilePath (inputDir : List String) (outputDir : List String)
    (path : List String) : FilePath :=
  let relativePath := path.diff inputDir
  outputDir ++ relativePath
    |> List.map (String.replace · ".lean" ".md")
    |> List.filter (· ≠ ".")
    |> ofComponents

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

/-- count how many `..` operator is applied
before reach `parentDir` from `childFile` -/
def relativePath (childFile parentDir : FilePath) : List String :=
  let childComponents := childFile.components
  let parentComponents := parentDir.components
  childComponents.diff parentComponents
    |>.map (fun _ => "..")

end System.FilePath
