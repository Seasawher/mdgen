import Mdgen.File

open System FilePath

/-- intermediate data structure which is annotated syntax infomation -/
structure RichLine where
  /-- text content -/
  content : String

  /-- nest level -/
  level : Nat

  /-- whether the line ends with the closing symbol or not. -/
  close : Bool
deriving Repr, BEq, Inhabited

instance : ToString RichLine where
  toString := RichLine.content

/-- handle ignore pattern -/
def filterIgnored (lines : Array String) : Array String := Id.run do
  let mut res := #[]
  let mut ignore := false
  for line in lines do
    if line.endsWith "--#" then
      continue
    if line.endsWith "--#--" then
      ignore := ! ignore
      continue
    if ignore then
      continue
    res := res.push line
  return res

/-- Receive a list of codes and count the nesting of block and sectioning comments.
The corresponding opening and closing brackets should have the same level.
-/
def analyze (lines : Array String) : List RichLine := Id.run do
  let lines := filterIgnored lines
  let mut res : List RichLine := []
  let mut level := 0
  let mut doc := false
  let mut blockCommentInDoc := false
  for line in lines do
    if line.startsWith "/--" then
      doc := true
    if line.startsWith "/-" && ! line.startsWith "/--" then
      if ! doc then
        level := level + 1
      else
        blockCommentInDoc := true
    res := {content := line, level := level, close := line.endsWith "-/" && ! doc} :: res
    if line.endsWith "-/" then
      if ! doc then
        level := level - 1
      if ! blockCommentInDoc then
        doc := false
      blockCommentInDoc := false
  return res.reverse

namespace Analyze

set_option linter.unusedVariables false in

/-- test for `analyze` function -/
def runTest (title := "") (input : Array String) (expected : List (Nat × Bool))  : IO Unit := do
  let output := analyze input |>.map (fun x => (x.level, x.close))
  if output ≠ expected then
    throw <| .userError s!"Test failed: \n{output}"

#eval runTest
  (title := "nested block comment")
  #[
    "/-",
      "/- inline -/",
      "/- multi",
      "line -/",
      "hoge",
    "-/",
    "foo"
  ]
  [(1, false), (2, true), (2, false), (2, true), (1, false), (1, true), (0, false)]

#eval runTest
  (title := "sectioning comment and nested block comment")
  #[
    "/-! hoge fuga",
      "/- foo! -/",
    "-/",
    "def foo := 1"
  ]
  [(1, false), (2, true), (1, true), (0, false)]

#eval runTest
  (title := "one line doc comment")
  #[
    "/-- hoge -/",
    "def hoge := \"hoge\""
  ]
  [(0, false), (0, false)]

#eval runTest
  (title := "multi line doc comment")
  #[
    "/-- hoge",
    "fuga -/",
    "def hoge := 42"
  ]
  [(0, false), (0, false), (0, false)]

#eval runTest
  (title := "raw code block")
  #[
    "/-",
      "```lean",
      "/-- greeting -/",
      "def foo := \"Hello World!\"",
      "```",
    "-/"
  ]
  [(1, false), (1, false), (1, false), (1, false), (1, false), (1, true)]

#eval runTest
  (title := "multi line ignoring")
  #[
    "--#--",
    "this is ignored",
    "this is also ignored",
    "--#--",
    "hoge"
  ]
  [(0, false)]

end Analyze
