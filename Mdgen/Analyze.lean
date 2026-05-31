module

import Mdgen.File
import all Mdgen.String

open System FilePath

/-- intermediate data structure which is annotated syntax infomation -/
public structure RichLine where
  /-- text content -/
  content : String

  /-- nest level -/
  level : Nat

  /-- whether the line ends with the closing symbol or not. -/
  close : Bool

  /-- whether the line should be ignored when buiding `Block` -/
  missing : Bool := false
deriving Repr, BEq, Inhabited

instance : ToString RichLine where
  toString := RichLine.content

/-- handle ignore pattern -/
public def filterIgnored (lines : Array String) : Array String := Id.run do
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

/-- Ensure `idx` is present in `indexes`. -/
private def Array.ensureIdx (indexes : Array Nat) (idx : Nat) : Array Nat :=
  if indexes.contains idx then
    indexes
  else
    indexes.push idx

/-- Find the start line of a doc comment ending at the last line in `lines`. -/
private def findDocCommentStart? (lines : Array String) : Option Nat :=
  let rec go : List (String × Nat) → Option Nat
    | [] => none
    | (line, idx) :: rest =>
      let trimmed := line.trimAsciiStart
      if trimmed.startsWith "/--" then
        some idx
      else if trimmed.startsWith "/-" then
        none
      else
        go rest

  let indexed := lines.zipIdx.toList.reverse
  match indexed with
  | [] => none
  | (last, _) :: _ =>
    if !(last.trimAscii.endsWith "-/") then
      none
    else
      go indexed

/-- preprocess for converting doc comment to block comment

#### Return
* `indexes`: the indexes of lines which are converted from doc comment to block comment.
* `contents`: the contents of lines after preprocessing.
-/
public def preprocessForDocToBlock (lines : Array String) : Array Nat × Array String := Id.run do
  let token := "/-⋆-//--"
  let mut indexes : Array Nat := #[]
  let mut contents : Array String := #[]
  let mut ignore := false
  for line in lines do
    let ignoreLine := line.endsWith "--#"
    if line.endsWith "--#--" then
      ignore := ! ignore
      continue
    if ignore then
      continue

    if ignoreLine && line.trimAsciiStart.startsWith "#guard_msgs" then
      match findDocCommentStart? contents with
      | none => pure ()
      | some idx => indexes := indexes.ensureIdx idx

    if ignoreLine then
      continue

    let idx := contents.size
    if line.trimAsciiStart.startsWith token then
      indexes := indexes.ensureIdx idx
      contents := contents.push (line.replace token "/--")
    else
      contents := contents.push line
  return (indexes, contents)

/-- postprocess for converting doc comment to block comment -/
public def postprocessForDocToBlock (indexes : Array Nat) (i : Nat) (line : String) : String :=
  if indexes.contains i then
    line.replaceFirst "/--" "/-"
  else
    line

/-- Receive a array of codes and count the nesting of block and sectioning comments.
The corresponding opening and closing brackets should have the same level.
-/
public def analyze (lines : Array String) : List RichLine := Id.run do
  let (indexes, lines) := preprocessForDocToBlock lines
  let mut res : List RichLine := []
  let mut level := 0
  let mut doc := false
  let mut blockCommentInDoc := false
  for (line, i) in lines.zipIdx do
    if line.startsWith "/--" then
      doc := true
    if line.startsWith "/-" && ! line.startsWith "/--" then
      if ! doc then
        level := level + 1
      else
        blockCommentInDoc := true
    let newLine := postprocessForDocToBlock indexes i line
    res := {content := newLine, level := level, close := line.endsWith "-/" && ! doc} :: res
    if line.endsWith "-/" then
      if ! doc then
        level := level - 1
      if ! blockCommentInDoc then
        doc := false
      blockCommentInDoc := false
  return res.reverse



set_option linter.unusedVariables false in

/-- test for `analyze` function -/
private def runTest (title := "") (input : Array String) (expected : List (Nat × Bool))  : IO Unit := do
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

#eval runTest
  (title := "empty lines")
  #[
    "",
    ""
  ]
  [(0, false), (0, false)]
