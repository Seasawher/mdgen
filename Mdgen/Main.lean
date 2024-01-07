import Mdgen.Base
import Mdgen.ConvertToMd

open Lake DSL System

private partial def getLeanFilePaths (fp : FilePath) (acc : Array FilePath := #[]) :
    IO $ Array FilePath := do
  if ← fp.isDir then
    (← fp.readDir).foldlM (fun acc dir => getLeanFilePaths dir.path acc) acc
  else return if fp.extension == some "lean" then acc.push fp else acc

/-- create a file with given path and content. -/
private def createFile (path : FilePath) (content : String) : IO Unit := do
  match path.parent with
  | none => IO.FS.writeFile path content
  | some parent =>
    IO.FS.createDirAll parent
    IO.FS.writeFile path content

private def outputFilePath (outputDir : FilePath) (path : FilePath) : FilePath :=
  path.components.diff outputDir.components
    |> List.drop 1
    |> ( outputDir.components ++ ·)
    |> List.map (· ++ FilePath.pathSeparator.toString)
    |> List.foldl (· ++ ·) ""
    |> (String.dropRight · 1)
    |> (String.replace · ".lean" ".md")
    |> FilePath.mk

def main (args : List String) : IO UInt32 := do
  if args.length != 2 then
    IO.eprintln s!"usage: mdgen <input_dir> <output_dir>"
    return 1

  let inputDir : FilePath := args.get! 0
  let outputDir : FilePath := args.get! 1

  let paths ← getLeanFilePaths inputDir

  for path in paths do
    let content ← IO.FS.lines path

    let outputFilePath := outputFilePath outputDir path

    createFile outputFilePath $ convertToMd content.toList
  return 0
