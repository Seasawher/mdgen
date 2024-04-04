open System

/-- new notaion to represent `x := x ++ e`. -/
syntax ident "++=" term : doElem

macro_rules
  | `(doElem| $x:ident ++= $e:term) => `(doElem| ($x) := ($x) ++ ($e))

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
  for line in lines do
    if line.endsWith "--#" then
      continue
    if line.startsWith "/-" && ! line.startsWith "/--" then
      level := level + 1
    res := {content := line, level := level, close := line.endsWith "-/"} :: res
    if line.endsWith "-/" then
      level := level - 1
  return res.reverse

namespace analysis

def runTest (input : List String) (expected : List (Nat × Bool)) (title := "") : IO Unit :=
  let output := analysis input
  if output.map (fun x => (x.level, x.close)) = expected then
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
  [(1, false), (2, false), (2, true), (2, false), (2, true), (1, false), (1, true), (0, false)]

#eval runTest
  (title := "sectioning comment and nested block comment")
  [
    "/-! hoge fuga",
      "/- foo! -/",
    "-/",
    "def foo := 1",
  ]
  [(1, false), (2, true), (1, true), (0, false)]

#eval runTest
  (title := "one line doc comment")
  [
    "/-- hoge -/",
    "def hoge := \"hoge\"",
  ]
  [(0, true), (0, false)]

#eval runTest
  (title := "multi line doc comment")
  [
    "/-- hoge",
    "fuga -/",
    "def hoge := 42",
  ]
  [(0, false), (0, true), (0, false)]

end analysis

/-- A chunk of grouped code for conversion to markdown. -/
structure Block where
  content : String
  toCodeBlock : Bool
  deriving Repr

instance : ToString Block where
  toString := fun b =>
    s!"content: \n{b.content}\n toCodeBlock: {b.toCodeBlock}\n\n"

def listShift {α : Type} (x : List α × List α) : List α × List α :=
  let ⟨l, r⟩ := x
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
        listShift <| lines.span (fun x => x.level > 1 || ! x.close)
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

instance : ToString Block where
  toString := fun b =>
    s!"content: \n{b.content}\n toCodeBlock: {b.toCodeBlock}\n\n"

private def mergeBlocks (blocks : List Block) : Md :=
  let res := blocks
    |>.map Block.toMd
    |>.foldl (· ++ ·) ""
  res.trim ++ "\n"

/-- convert lean contents to markdown contents. -/
def convertToMd (lines : List String) : Md :=
  let blocks := buildBlocks <| analysis lines
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
  (title := " consecutive single-line block comments.")
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

end ConvertToMd
