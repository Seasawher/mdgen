import Lake

open Lake DSL System

structure Block where
  content : String
  isCode : Bool

private def buildBlocks(lines : List String) : List Block := Id.run do
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
        content := (content.splitOn "-/")[0]!
        blocks := blocks ++ [{content := content.trim, isCode := false}]
        readingLeanCode := true
        content := ""
    else if line.endsWith "-/" && ! readingLeanCode then
      content := content ++ (line.splitOn "-/")[0]!
      readingLeanCode := true
      blocks := blocks ++ [{content := content.trim, isCode := false}]
      content := ""
    else
      content := content ++ line ++ "\n"
  if content != "" then
    blocks := blocks ++ [{content := content.trim, isCode := true}]
  return blocks

private def mergeBlocks (blocks : List Block) : String := Id.run do
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

def List.withBreakLine (as : List String) : String :=
  as.map (· ++ "\n") |> foldl (· ++ ·) ""

def inlineComment := ["-- this is a test"]

def inlineCommentExpected := [
  "```lean",
  "-- this is a test",
  "```"
].withBreakLine

example : convertToMd inlineComment = inlineCommentExpected := rfl

def sectionComment := ["/-! # This is a test -/"]

def sectionCommentExpected := ["# This is a test"].withBreakLine

example : convertToMd sectionComment = sectionCommentExpected := rfl

def sectionCommentMulti := [
  "/-! # This is a test",
  "of multiline section comment -/"
]

def sectionCommentMultiExpected := [
  "# This is a test",
  "of multiline section comment"
].withBreakLine

example : convertToMd sectionCommentMulti = sectionCommentMultiExpected := rfl

def manyEmptyLines := ["/-! test -/", "", "", ""]

def manyEmptyLinesExpected := ["test"].withBreakLine

example : convertToMd manyEmptyLines = manyEmptyLinesExpected := rfl

def ignoreLine := ["this is ignored --#", "this is also ignored --#"]

def ignoreLineExpected := "\n"

example : convertToMd ignoreLine = ignoreLineExpected := rfl

def docComment := ["/-- This is a test -/", "def foo := 0"]

def docCommentExpected := [
  "```lean",
  "/-- This is a test -/",
  "def foo := 0",
  "```"
].withBreakLine

example : convertToMd docComment = docCommentExpected := rfl

def docCommentMulti := [
  "/-- This is a test",
  "of multiline doc comment -/",
  "def foo := 0"
]

def docCommentMultiExpected := [
  "```lean",
  "/-- This is a test",
  "of multiline doc comment -/",
  "def foo := 0",
  "```"
].withBreakLine

example : convertToMd docCommentMulti = docCommentMultiExpected := rfl

def blockComment := ["/- this is a test -/"]

def blockCommentExpected := ["this is a test"].withBreakLine

example : convertToMd blockComment = blockCommentExpected := rfl

def blockCommentMulti := [
  "/-",
  "this is a test",
  "of multiline block comment -/"
]

def blockCommentMultiExpected := [
  "this is a test",
  "of multiline block comment"
].withBreakLine

example : convertToMd blockCommentMulti = blockCommentMultiExpected := rfl
