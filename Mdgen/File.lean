module

public meta import Init.System.FilePath
import all Mdgen.List

open System FilePath

/-- copy the content of a file from `src` to `dst`.
This function do't use `String` operations so that it works for binary files as well. -/
public def IO.FS.copyFile (src dst : FilePath) : IO Unit := do
  let bytes ← IO.FS.readBinFile src
  if let some parent := dst.parent then
    IO.FS.createDirAll parent
  IO.FS.writeBinFile dst bytes

namespace System.FilePath

/-- Checks if a file path has a `.lean` extension. -/
public def isLeanFile (path : FilePath) : Bool :=
  String.toLower <$> path.extension == some "lean"

/-- Checks if a file path has a `.md` extension. -/
public def isMarkdownFile (path : FilePath) : Bool :=
  String.toLower <$> path.extension == some "md"

/-- a function which returns an output file path component
given components of input and output directories. -/
public def mkOutputFilePath (inputDir outputDir path : FilePath) : FilePath :=
  let relativePath := path.components.diff inputDir.components
  let fp : FilePath := outputDir.components
    |> (· ++ relativePath)
    |> List.filter (· ≠ ".")
    |> mkFilePath
  if fp.extension == some "lean" then
    fp.withExtension "md"
  else
    fp

#guard mkOutputFilePath "." "out" "foo.lean" = mkFilePath ["out", "foo.md"]

#guard mkOutputFilePath "src" "." "src/foo.lean" = mkFilePath ["foo.md"]

#guard mkOutputFilePath "src" "out" "src/foo.lean" = mkFilePath ["out", "foo.md"]

#guard mkOutputFilePath "test/src" "test/out" "test/src/foo.lean" = mkFilePath ["test", "out", "foo.md"]

#guard mkOutputFilePath "src" "out/dist" "src/foo/bar.lean" = mkFilePath ["out", "dist", "foo", "bar.md"]

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
  childComponents.diff parentComponents |>.map (fun _ => "..")

end System.FilePath
