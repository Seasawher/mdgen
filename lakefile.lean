import Lake
open Lake DSL

package «mdgen» where
  -- add package configuration options here

lean_lib «Mdgen» where
  -- add library configuration options here

@[default_target]
lean_exe «mdgen» where
  root := `Main

section md_gen

open System

partial def getLeanFilePaths (fp : FilePath) (acc : Array FilePath := #[]) :
    IO $ Array FilePath := do
  if ← fp.isDir then
    (← fp.readDir).foldlM (fun acc dir => getLeanFilePaths dir.path acc) acc
  else return if fp.extension == some "lean" then acc.push fp else acc

def convertToMd (content : String) : String := Id.run do
  return content

/-- create a file with given path and content. -/
def createFile (path : FilePath) (content : String) : IO Unit := do
  match path.parent with
  | none => IO.FS.writeFile path content
  | some parent =>
    IO.FS.createDirAll parent
    IO.FS.writeFile path content

script md_gen (args : List String) do

  if args.length != 2 then
    IO.eprintln s!"usage: md_gen <input_dir> <output_dir>"
    return 1

  let inputDir : FilePath := args.get! 0
  let outputDir : FilePath := args.get! 1

  let paths := ← getLeanFilePaths inputDir

  for path in paths do
    let content ← IO.FS.readFile path

    let outputFilePath := outputDir.toString
      |> ( · :: path.components.drop 1)
      |> List.map (· ++ FilePath.pathSeparator.toString)
      |> List.foldl (· ++ ·) ""
      |> (String.dropRight · 1)
      |> (String.replace · ".lean" ".md")
      |> FilePath.mk

    IO.println s!"outputFilePath: {outputFilePath}"
    createFile outputFilePath $ convertToMd content
  return 0

end md_gen
