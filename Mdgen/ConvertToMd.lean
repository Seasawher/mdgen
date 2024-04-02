open System

/-- new notaion to represent `x := x ++ e`. -/
syntax ident "++=" term : doElem

macro_rules
  | `(doElem| $x:ident ++= $e:term) => `(doElem| ($x) := ($x) ++ ($e))

/-- A chunk of grouped code for conversion to markdown. -/
structure Block where
  content : String
  toCodeBlock : Bool
  deriving Repr

private def buildBlocks (lines : List String) : List Block := Id.run do
  let mut readingLeanCode := true
  let mut blocks : List Block := []
  let mut content := ""
  for ⟨i, line_with_br⟩ in lines.enum do
    let line := (line_with_br.splitOn "\n")[0]!
    if line.endsWith "--#" then
      continue
    if line.startsWith "/-" && ! line.startsWith "/--" then
      if ! readingLeanCode then
        panic!
          "Nested lean commentary sections not allowed in:\n" ++
          s!" line {i+1}: {line}"
      blocks ++= [{content := content.trim, toCodeBlock := true}]
      readingLeanCode := false
      content := line ++ "\n"
      if line.endsWith "-/" then
        blocks ++= [{content := content.trim, toCodeBlock := false}]
        readingLeanCode := true
        content := ""
    else if line.endsWith "-/" && ! readingLeanCode then
      content ++= line
      readingLeanCode := true
      blocks ++= [{content := content.trim, toCodeBlock := false}]
      content := ""
    else
      content ++= line ++ "\n"
  if content != "" then
    blocks ++= [{content := content.trim, toCodeBlock := true}]
  return blocks

private def Block.toString (b : Block) : String :=
  if b.content == "" then
    ""
  else if b.toCodeBlock then
    "```lean\n" ++ b.content ++ "\n```\n\n"
  else
    let separator := if b.content.startsWith "/-!" then "/-!" else "/-"
    b.content
      |> (String.splitOn · separator)
      |> List.drop 1
      |> List.foldl (· ++ ·) ""
      |> (String.splitOn · "-/")
      |> List.dropLast
      |> List.foldl (· ++ ·) ""
      |> String.trim
      |> (· ++ "\n\n")

private def mergeBlocks (blocks : List Block) : String :=
  let res := blocks
    |>.map Block.toString
    |>.foldl (· ++ ·) ""
  res.trim ++ "\n"

/-- convert lean contents to markdown contents. -/
def convertToMd (lines : List String) : String :=
  let blocks := buildBlocks lines
  mergeBlocks blocks

namespace ConvertToMd

def _root_.List.withBreakLine (as : List String) : String :=
  as.map (· ++ "\n") |>.foldl (· ++ ·) ""

def runTest (input : List String) (expected : List String) (title := "") : IO Unit :=
  let output := convertToMd input
  if output = expected.withBreakLine then
    IO.println s!"{title} test passed!"
  else
    throw <| .userError s!"Test failed: {output}"

#eval runTest
  (title := "inline comment")
  ["-- this is a test"]
  [
    "```lean",
    "-- this is a test",
    "```"
  ]

#eval runTest
  (title := "module document")
  ["/-! # This is a test -/"]
  ["# This is a test"]

#eval runTest
  (title := "multi line sectioning comment")
  [
    "/-! # This is a test",
    "of multiline section comment -/"
  ]
  [
    "# This is a test",
    "of multiline section comment"
  ]

#eval runTest
  (title := "empty lines")
  ["/-! test -/", "", "", ""]
  ["test"]

#eval runTest
  (title := "ignored lines")
  ["this is ignored --#", "this is also ignored --#"]
  [""]

#eval runTest
  (title := "doc comment")
  [
    "/-- This is a test -/",
    "def foo := 0"
  ]
  [
    "```lean",
    "/-- This is a test -/",
    "def foo := 0",
    "```"
  ]

#eval runTest
  (title := "multi line doc comment")
  [
    "/-- This is a test",
    "of multiline doc comment -/",
    "def foo := 0"
  ]
  [
    "```lean",
    "/-- This is a test",
    "of multiline doc comment -/",
    "def foo := 0",
    "```"
  ]

#eval runTest
  (title := "block comment")
  ["/- this is a test -/"]
  ["this is a test"]

#eval runTest
  (title := "multi line block comment")
  [
    "/-",
    "this is a test",
    "of multiline block comment -/"
  ]
  [
    "this is a test",
    "of multiline block comment"
  ]

end ConvertToMd
