open System

/-- new notaion to represent `x := x ++ e`. -/
syntax ident "++=" term : doElem

macro_rules
  | `(doElem| $x:ident ++= $e:term) => `(doElem| ($x) := ($x) ++ ($e))

structure LeveledLine where
  /-- text content -/
  content : String

  /-- nest level -/
  level : Nat
  deriving Repr, BEq

instance : ToString LeveledLine where
  toString := fun l => s!"content: \n{l.content}\n, level: {l.level}"

/-- Receive a list of codes and count the nesting of block and sectioning comments.
* The corresponding opening and closing brackets should have the same level.
* Also handles the exclusion of ignored targets.
-/
def analysis (lines : List String) : List LeveledLine := Id.run do
  let mut level := 0
  let mut res : List LeveledLine := []
  for line in lines do
    if line.endsWith "--#" then
      continue
    if line.startsWith "/-" && ! line.startsWith "/--" then
      level := level + 1
    res := {content := line, level := level} :: res
    if line.endsWith "-/" then
      level := level - 1
  return res.reverse

namespace analysis

def runTest (input : List String) (expected : List Nat) (title := "") : IO Unit :=
  let output := analysis input
  if output.map (·.level) = expected then
    IO.println s!"{title} test passed!"
  else
    throw <| .userError s!"Test failed: \n{output}"

#eval runTest
  (title := "nested block comment")
  [
    "/-",
      "/- hoge",
      "-/",
      "/-",
      "-/",
      "hoge",
    "-/",
    "foo"
  ]
  [1, 2, 2, 2, 2, 1, 1, 0]

#eval runTest
  (title := "sectioning comment and nested block comment")
  [
    "/-! hoge fuga",
      "/- foo! -/",
    "-/",
    "def foo := 1",
  ]
  [1, 2, 1, 0]

#eval runTest
  (title := "one line doc comment")
  [
    "/-- hoge -/",
    "def hoge := \"hoge\"",
  ]
  [0, 0]

#eval runTest
  (title := "multi line doc comment")
  [
    "/-- hoge",
    "fuga -/",
    "def hoge := 42",
  ]
  [0, 0, 0]

end analysis

/-- A chunk of grouped code for conversion to markdown. -/
structure Block where
  content : String
  toCodeBlock : Bool
  deriving Repr

instance : ToString Block where
  toString := fun b =>
    s!"content: \n{b.content}\n toCodeBlock: {b.toCodeBlock}\n\n"

partial def buildBlocks (lines : List LeveledLine) : List Block :=
  match lines with
  | [] => []
  | ⟨_, level₀⟩ :: _ =>
    let (fst, snd) := if level₀ == 0
        then lines.span (·.level == 0)
        else lines.span (·.level >= 1)
    let fstBlock : Block := {
      content := fst.map (·.content)
        |>.map (· ++ "\n")
        |>.foldl (· ++ ·) ""
        |>.trim,
      toCodeBlock := (level₀ == 0)
    }
    fstBlock :: buildBlocks snd

#eval (buildBlocks <| analysis ["/-", "foo", "bar", "-/", "/-", "baz", "-/"]) |>.map (·.content)

/-- markdown text -/
abbrev Md := String

private def Block.toMd (b : Block) : Md :=
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

#eval Block.toMd {
    content := "/-\nfoo\nbar\n-/\n/-\nbaz\n-/"
    toCodeBlock := false
  }

instance : ToString Block where
  toString := fun b =>
    s!"content: \n{b.content}\n toCodeBlock: {b.toCodeBlock}\n\n"

#eval Block.toMd {
    content := "/-! # This is a test \n/-\nhoge\n-/\n-/",
    toCodeBlock := false
  }

private def mergeBlocks (blocks : List Block) : Md :=
  let res := blocks
    |>.map Block.toMd
    |>.foldl (· ++ ·) ""
  res.trim ++ "\n"

/-- convert lean contents to markdown contents. -/
def convertToMd (lines : List String) : Md :=
  let blocks := buildBlocks <| analysis lines
  dbg_trace s!"blocks: {blocks}"
  mergeBlocks blocks

namespace ConvertToMd

def _root_.List.withBreakLine (as : List String) : String :=
  as.map (· ++ "\n") |>.foldl (· ++ ·) ""

def runTest (input : List String) (expected : List String) (title := "") : IO Unit :=
  let output := convertToMd input
  if output = expected.withBreakLine then
    IO.println s!"{title} test passed!"
  else
    throw <| .userError s!"Test failed: \n{output}"

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

-- #eval runTest
--   (title := "raw code block")
--   [
--     "/-",
--     "```lean",
--     "/- this is test -/",
--     "```",
--     "-/",
--     "/- hoge -/",
--   ]
--   [
--     "```lean",
--     "/- this is test -/",
--     "```",
--     "hoge"
--   ]

end ConvertToMd
