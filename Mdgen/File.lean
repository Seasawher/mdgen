module

public meta import Init.System.FilePath
import all Mdgen.List

open System FilePath

namespace System.FilePath

/-- a function which returns an output file path component
given components of input and output directories. -/
public def outputFilePath (inputDir outputDir path : FilePath) : FilePath :=
  let relativePath := path.components.diff inputDir.components
  outputDir.components
    |> (· ++ relativePath)
    |> List.map (String.replace · ".lean" ".md")
    |> List.filter (· ≠ ".")
    |> mkFilePath

#guard outputFilePath "." "out" "foo.lean" = mkFilePath ["out", "foo.md"]

#guard outputFilePath "src" "." "src/foo.lean" = mkFilePath ["foo.md"]

#guard outputFilePath "src" "out" "src/foo.lean" = mkFilePath ["out", "foo.md"]

#guard outputFilePath "test/src" "test/out" "test/src/foo.lean" = mkFilePath ["test", "out", "foo.md"]

#guard outputFilePath "src" "out/dist" "src/foo/bar.lean" = mkFilePath ["out", "dist", "foo", "bar.md"]

/--
Recursively get all file paths contained in a directory whose path is `path`.
If `path` is a file, return an array containing only `path`.
-/
public partial def getAllFilePaths (path : FilePath) : IO (Array FilePath) := do
  let mut results : Array FilePath := #[]
  if ← path.isDir then
    let entries ← path.readDir
    for entry in entries do
      results := results ++ (← getAllFilePaths entry.path)
    return results
  else
    return #[path]

/-- Recursively get all Lean file paths contained in a directory whose path is `path`.
If `path` is a file, return an array containing only `path` if it is a Lean file. -/
public def getAllLeanFilePaths (path : FilePath) : IO (Array FilePath) := do
  let allFiles ← getAllFilePaths path
  return allFiles.filter (fun fp => fp.extension == some "lean")

/-- create a file with given path and content. -/
public def createFile (path : FilePath) (content : String) : IO Unit := do
  match path.parent with
  | none => IO.FS.writeFile path content
  | some parent =>
    IO.FS.createDirAll parent
    IO.FS.writeFile path content

/-- count how many `..` operator is applied
before reach `parentDir` from `childFile` -/
public def relativePath (childFile parentDir : FilePath) : List String :=
  let childComponents := childFile.components
  let parentComponents := parentDir.components
  childComponents.diff parentComponents
    |>.map (fun _ => "..")

end System.FilePath
