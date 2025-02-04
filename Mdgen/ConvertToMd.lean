import Mdgen.Analyze

/-- A chunk of grouped code for conversion to markdown. -/
structure Block where
  /-- content of block -/
  content : String

  /-- whether the `content` is converted into code section in markdown -/
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

/-- build a `Block` from a `RichLine` -/
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

/-- convert a `Block` intro a markdown snippet -/
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

/-- merge blocks and build a markdown content -/
def mergeBlocks (blocks : List Block) : Md :=
  let res := blocks
    |>.map Block.toMd
    |>.foldl (· ++ ·) ""
  res.trim ++ "\n"

open System FilePath

/-- Handle uniform internal link syntax.
This converts `#{root}` in internal link to repeated `../` string -/
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
  let blocks := buildBlocks <| analyze lines

  let postProcessedBlocks :=
    match outputFilePath, outputDir with
    | some outputFilePath, some outputDir =>
      blocks.map (Block.postProcess outputFilePath outputDir)
    | _, _ => blocks

  mergeBlocks postProcessedBlocks

namespace ConvertToMd

/-- add breakline for each element in a list -/
def _root_.List.withBreakLine (as : List String) : String :=
  as.map (· ++ "\n") |>.foldl (· ++ ·) ""

set_option linter.unusedVariables false in

/-- test for `convertToMd` -/
def runTest (input : List String) (expected : List String) (title := "") : IO Unit := do
  let output := convertToMd (lines := input)
  if output ≠ expected.withBreakLine then
    throw <| .userError s!"test for \"{title}\" failed: \n{output}"

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
  (title := "block comment in doc comment")
  [
    "/-- foo",
    "bar /- hoge -/",
    "baz -/",
  ]
  [
    "```lean",
    "/-- foo",
    "bar /- hoge -/",
    "baz -/",
    "```"
  ]

#eval runTest
  (title := "leading block comment in doc comment")
  [
    "/-- foo",
    "/- hoge bar -/",
    "baz -/",
  ]
  [
    "```lean",
    "/-- foo",
    "/- hoge bar -/",
    "baz -/",
    "```"
  ]

#eval runTest
  (title := "multiple leading block comments in doc comment")
  [
    "/-- foo",
    "/- hoge bar -/",
    "/- fuga -/ baz",
    "baz -/",
  ]
  [
    "```lean",
    "/-- foo",
    "/- hoge bar -/",
    "/- fuga -/ baz",
    "baz -/",
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
    "of multiline block comment -/",
  ]
  [
    "this is a test",
    "of multiline block comment"
  ]

#eval runTest
  (title := "respect indent")
  [
    "hoge",
    "  fuga",
    "  monyo",
  ]
  [
    "```lean",
    "hoge",
    "  fuga",
    "  monyo",
    "```"
  ]

#eval runTest
  (title := "consecutive single-line block comments.")
  [
    "/- hoge -/",
    "/- fuga -/",
  ]
  [
    "hoge",
    "",
    "fuga"
  ]

#eval runTest
  (title := "nested block comment")
  [
    "/-",
    "this is a test",
    "/- nested comment -/",
    "of nested block comment -/"
  ]
  [
    "this is a test",
    "/- nested comment -/",
    "of nested block comment"
  ]

#eval runTest
  (title := "raw code block")
  [
    "/-",
    "```lean",
    "/- this is test -/",
    "```",
    "fuga",
    "-/",
    "/- hoge -/",
  ]
  [
    "```lean",
    "/- this is test -/",
    "```",
    "fuga",
    "",
    "hoge"
  ]

#eval runTest
  (title := "indent in raw code block")
  [
    "/-",
    "```lean",
    "  hoge",
    "```",
    "-/"
  ]
  [
    "```lean",
    "  hoge",
    "```"
  ]

#eval runTest
  (title := "doc comment in raw code block")
  [
    "/-",
    "```lean",
    "/-- foo -/",
    "def zero := 0",
    "```",
    "-/",
  ]
  [
    "```lean",
    "/-- foo -/",
    "def zero := 0",
    "```"
  ]

#eval runTest
  (title := "multiple raw code blocks")
  [
    "/-",
    "```lean",
    "/-- greeting -/",
    "def foo := \"Hello World!\"",
    "```",
    "",
    "```lean",
    "/-! ### second code block -/",
    "",
    "def one := 1",
    "```",
    "-/",
  ]
  [
    "```lean",
    "/-- greeting -/",
    "def foo := \"Hello World!\"",
    "```",
    "",
    "```lean",
    "/-! ### second code block -/",
    "",
    "def one := 1",
    "```",
  ]

end ConvertToMd
