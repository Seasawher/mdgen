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

namespace Analysis

set_option linter.unusedVariables false in

/-- test for `analysis` function -/
def runTest (title := "") (input : List String) (expected : List (Nat × Bool))  : IO Unit := do
  let output := analysis input |>.map (fun x => (x.level, x.close))
  if output ≠ expected then
    throw <| .userError s!"Test failed: \n{output}"

#eval runTest
  (title := "nested block comment")
  [
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
  [(0, false), (0, false)]

#eval runTest
  (title := "multi line doc comment")
  [
    "/-- hoge",
    "fuga -/",
    "def hoge := 42",
  ]
  [(0, false), (0, false), (0, false)]

#eval runTest
  (title := "raw code block")
  [
    "/-",
      "```lean",
      "/-- greeting -/",
      "def foo := \"Hello World!\"",
      "```",
    "-/",
  ]
  [(1, false), (1, false), (1, false), (1, false), (1, false), (1, true)]

#eval runTest
  (title := "multi line ignoring")
  [
    "--#--",
    "this is ignored",
    "this is also ignored",
    "--#--",
    "hoge",
  ]
  [(0, false)]

end Analysis
