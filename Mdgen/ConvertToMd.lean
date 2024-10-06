import Mdgen.File

open System FilePath

structure RichLine where
  /-- text content -/
  content : String

  /-- nest level -/
  level : Nat

  /-- whether the line ends with the closing symbol or not. -/
  close : Bool
  deriving Repr, BEq, Inhabited

instance : ToString RichLine where
  toString := fun l => l.content

/-- Receive a list of codes and count the nesting of block and sectioning comments.
* The corresponding opening and closing brackets should have the same level.
* Also handles the exclusion of ignored targets.
-/
def analysis (lines : List String) : List RichLine := Id.run do
  let mut res : List RichLine := []
  let mut level := 0
  let mut doc := false
  let mut ignore := false
  for line in lines do
    -- ignore pattern
    if line.endsWith "--#" then
      continue
    if line.endsWith "--#--" then
      ignore := ! ignore
      continue
    if ignore then
      continue

    if line.startsWith "/--" then
      doc := true
    if line.startsWith "/-" && ! line.startsWith "/--" then
      level := level + 1
    res := {content := line, level := level, close := line.endsWith "-/" && ! doc} :: res
    if line.endsWith "-/" then
      if ! doc then
        level := level - 1
      doc := false
  return res.reverse

/-- A chunk of grouped code for conversion to markdown. -/
structure Block where
  content : String
  toCodeBlock : Bool
  deriving Repr

instance : ToString Block where
  toString := fun b =>
    s!"content: \n{b.content}\n toCodeBlock: {b.toCodeBlock}\n\n"

/-- a variant of `List.span` which return a list including
at most one "edge" element -/
def List.spanWithEdge {α : Type} (p : α → Bool) (as : List α) : List α × List α :=
  let ⟨l, r⟩ := as.span p
  match r with
  | [] => (l, [])
  | y :: ys => (l ++ [y], ys)

partial def buildBlocks (lines : List RichLine) : List Block :=
  match lines with
  | [] => []
  | line :: _ =>
    let ⟨_, level, _⟩ := line
    let splited := (
      if level == 0 then
        lines.span (fun x => x.level == 0)
      else
        lines.spanWithEdge (fun x => x.level > 1 || ! x.close)
    )
    let fstBlock : Block := {
      content := splited.fst
        |>.map (·.content)
        |>.map (· ++ "\n")
        |>.foldl (· ++ ·) ""
        |>.trim,
      toCodeBlock := (level == 0)
    }
    fstBlock :: buildBlocks splited.snd

/-- markdown text -/
abbrev Md := String

def Block.toMd (b : Block) : Md :=
  if b.content == "" then
    ""
  else if b.toCodeBlock then
    "```lean\n" ++ b.content ++ "\n```\n\n"
  else
    let separator := if b.content.startsWith "/-!" then "/-!" else "/-"
    b.content
      |> (String.drop · separator.length)
      |> (String.dropRight · "-/".length)
      |> String.trim
      |> (· ++ "\n\n")

instance : ToString Block where
  toString := fun b =>
    s!"content: \n{b.content}\n toCodeBlock: {b.toCodeBlock}\n\n"

def mergeBlocks (blocks : List Block) : Md :=
  let res := blocks
    |>.map Block.toMd
    |>.foldl (· ++ ·) ""
  res.trim ++ "\n"

/-- convert `#{root}` in internal link to repeated `../` string -/
def Block.postProcess (outputFilePath outputDir : FilePath) (b : Block) : Block := Id.run do
  if b.toCodeBlock then
    return b

  let pathPrefix := relativePath outputFilePath outputDir
    |>.drop 1
    |>.map (· ++ "/")
    |>.foldl (· ++ ·) ""
  let newContent := b.content
    |>.replace "#{root}/" pathPrefix
  return {b with content := newContent}

/-- convert lean contents to markdown contents. -/
def convertToMd (outputFilePath outputDir : Option FilePath := none) (lines : List String) : Md :=
  let blocks := buildBlocks <| analysis lines

  let postProcessedBlocks :=
    match outputFilePath, outputDir with
    | some outputFilePath, some outputDir =>
      blocks.map (Block.postProcess outputFilePath outputDir)
    | _, _ => blocks

  mergeBlocks postProcessedBlocks
