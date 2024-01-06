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

def buildBlocks(lines : List String) : List Block := Id.run do
  let mut readingLeanCode := true
  let mut blocks : List Block := []
  let mut content := ""
  for ⟨i, line_n⟩ in lines.enum do
    let line := (line_n.splitOn "\n")[0]!
    if line.endsWith "--#" then
      continue
    if line.startsWith "/-" && ! line.startsWith "/--" then
      if ! readingLeanCode then
        panic!
          "Nested lean commentary sections not allowed in:\n" ++
          s!" line {i+1}: {line}"
      blocks := blocks ++ [{content := content.trim, isCode := true}]
      readingLeanCode := false
      content := (if line.startsWith "/-!" then "/-!" else "/-")
        |> (String.splitOn line ·)
        |> List.getLast!
        |> (· ++ "\n")
      if line.endsWith "-/" then
        readingLeanCode := true
        content := (content.splitOn "-/")[0]!
        blocks := blocks ++ [{content := content.trim, isCode := false}]
        content := ""
    else if line.endsWith "-/" && ! readingLeanCode then
      content := (line.splitOn "-/")[0]!
      readingLeanCode := true
      blocks := blocks ++ [{content := content.trim, isCode := false}]
      content := ""
    else
      content := content ++ line ++ "\n"
  if content != "" then
    blocks := blocks ++ [{content := content.trim, isCode := true}]
  return blocks

def mergeBlocks (blocks : List Block) : String := Id.run do
  let mut res := ""
  for block in blocks do
    if block.content = "" then
      continue
    if block.isCode then
      res := res ++ "```lean\n" ++ block.content ++ "\n```\n\n"
    else
      res := res ++ block.content ++ "\n\n"
  return res.trim ++ "\n"

/-- convert lean contents to markdown contents. -/
def convertToMd (lines : List String) : String := Id.run do
  let blocks := buildBlocks lines
  return mergeBlocks blocks

/-- create a file with given path and content. -/
def createFile (path : FilePath) (content : String) : IO Unit := do
  match path.parent with
  | none => IO.FS.writeFile path content
  | some parent =>
    IO.FS.createDirAll parent
    IO.FS.writeFile path content

/-- A new list created by removing the matching parts of two lists from the beginning. -/
def List.diff (as : List String) (bs : List String) : List String :=
  match as, bs with
  | [], [] => []
  | a, [] => a
  | [], _ => []
  | a :: as, b :: bs => if a == b then List.diff as bs else a :: as

example : List.diff ["test", "src", "first"] ["test", "out"] = ["src", "first"] := rfl

def outputFilePath (outputDir : FilePath) (path : FilePath) : FilePath :=
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
    IO.eprintln s!"usage: md_gen <input_dir> <output_dir>"
    return 1

  let inputDir : FilePath := args.get! 0
  let outputDir : FilePath := args.get! 1

  let paths ← getLeanFilePaths inputDir

  for path in paths do
    let content ← IO.FS.lines path

    let outputFilePath := outputFilePath outputDir path

    createFile outputFilePath $ convertToMd content.toList
  return 0
