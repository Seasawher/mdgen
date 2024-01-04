import Lake

open Lake DSL System

partial def getLeanFilePaths (fp : FilePath) (acc : Array FilePath := #[]) :
    IO $ Array FilePath := do
  if ← fp.isDir then
    (← fp.readDir).foldlM (fun acc dir => getLeanFilePaths dir.path acc) acc
  else return if fp.extension == some "lean" then acc.push fp else acc

structure Block where
  content : String
  isCode : Bool

def buildBlocks(lines : Array String) : String := Id.run do
  let mut readingLeanCode := true
  let mut blocks : List Block := []
  let mut content := ""
  -- for i, line_n in lines.enum
  return ""

/-- convert lean contents to markdown contents. -/
def convertToMd (lines : Array String) : String := Id.run do

  return lines.foldl (· ++ ·) ""

/-- create a file with given path and content. -/
def createFile (path : FilePath) (content : String) : IO Unit := do
  match path.parent with
  | none => IO.FS.writeFile path content
  | some parent =>
    IO.FS.createDirAll parent
    IO.FS.writeFile path content

def main (args : List String) : IO UInt32 := do

  if args.length != 2 then
    IO.eprintln s!"usage: md_gen <input_dir> <output_dir>"
    return 1

  let inputDir : FilePath := args.get! 0
  let outputDir : FilePath := args.get! 1

  let paths := (← getLeanFilePaths inputDir)

  for path in paths do
    let content ← IO.FS.lines path

    let outputFilePath := outputDir.toString
      |> ( · :: path.components.drop 1)
      |> List.map (· ++ FilePath.pathSeparator.toString)
      |> List.foldl (· ++ ·) ""
      |> (String.dropRight · 1)
      |> (String.replace · ".lean" ".md")
      |> FilePath.mk

    createFile outputFilePath $ convertToMd content
  return 0
