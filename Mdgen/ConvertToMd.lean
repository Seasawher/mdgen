import Mdgen.Analyze
import Mdgen.String

/-- A chunk of grouped code for conversion to markdown. -/
structure Block where
  /-- content of block -/
  content : String

  /-- whether the `content` is converted into code section in markdown.

  * `none` means the content is not a code block.
  * `some lang` means the content is a code block with the given language.
  -/
  codeBlock : Option String
  deriving Repr

/-- a variant of `List.span` which return a list including
at most one "edge" element -/
def List.spanWithEdge {α : Type} (p : α → Bool) (as : List α) : List α × List α :=
  let ⟨l, r⟩ := as.span p
  match r with
  | [] => (l, [])
  | y :: ys => (l ++ [y], ys)

/-- Preprocess `RichLine` and extract metadata to be attached to code blocks -/
def RichLine.preprocess (line : RichLine) : RichLine × Option String :=
  if line.content.startsWith "-- ⋆MDGEN_LANG⋆=" then
    let lang := line.content.drop "-- ⋆MDGEN_LANG⋆=".length
    ({line with content := ""}, some lang)
  else
    (line, none)

/-- build a `Block` from a `RichLine` -/
partial def buildBlocks (lines : List RichLine) : List Block :=
  helper lines []
where
  helper (lines : List RichLine) (acc : List Block) : List Block :=
    match lines with
    | [] => acc.reverse
    | line :: rest =>
      let (line, lang?) := line.preprocess
      let lines := line :: rest

      let ⟨_, level, _⟩ := line
      let splited := (
        if level == 0 then
          lines.span (fun x => x.level == 0)
        else
          lines.spanWithEdge (fun x => x.level > 1 || ! x.close)
      )
      let fstBlock : Block := {
        content := splited.fst
          |>.map (·.content ++ "\n")
          |>.foldl (· ++ ·) ""
          |>.trim,
        codeBlock := if level == 0 then some (lang?.getD "lean") else none
      }
      helper splited.snd (fstBlock :: acc)

/-- convert a `Block` intro a markdown snippet -/
protected def Block.toString (b : Block) : String := Id.run do
  if b.content == "" then
    return ""

  match b.codeBlock with
  | some lang => s!"```{lang}\n" ++ b.content ++ "\n```\n\n"
  | none =>
    let separator := if b.content.startsWith "/-!" then "/-!" else "/-"
    b.content
      |> (String.drop · separator.length)
      |> (String.dropRight · "-/".length)
      |> String.trim
      |> (· ++ "\n\n")

/-- merge blocks and build a markdown content -/
def mergeBlocks (blocks : List Block) : String :=
  let res := blocks
    |>.map Block.toString
    |>.foldl (· ++ ·) ""
  res.trim ++ "\n"

open System FilePath

/-- Handle uniform internal link syntax.
This converts `#{root}` in internal link to repeated `../` string -/
def Block.postProcess (outputFilePath outputDir : FilePath) (b : Block) : Block := Id.run do
  if b.codeBlock.isSome then
    return b

  let pathPrefix := relativePath outputFilePath outputDir
    |>.drop 1
    |>.map (· ++ "/")
    |>.foldl (· ++ ·) ""
  let newContent := b.content
    |>.replace "#{root}/" pathPrefix
  return {b with content := newContent}

/-- convert lean contents to markdown contents. -/
def convertToMd (outputFilePath outputDir : Option FilePath := none) (lines : Array String) : String :=
  let blocks := buildBlocks <| analyze lines

  let postProcessedBlocks :=
    match outputFilePath, outputDir with
    | some outputFilePath, some outputDir =>
      blocks.map (Block.postProcess outputFilePath outputDir)
    | _, _ => blocks

  mergeBlocks postProcessedBlocks



set_option linter.unusedVariables false in

/-- test for `convertToMd` -/
private def runTest (input : Array String) (expected : String) (title := "") : IO Unit := do
  let output := convertToMd (lines := input)
  if output ≠ expected then
    throw <| .userError s!"test for \"{title}\" failed: \n{output}"

#eval runTest
  (title := "inline comment")
  #["-- this is a test"]
  [str|
    "```lean",
    "-- this is a test",
    "```"
  ]

#eval runTest
  (title := "module document")
  #["/-! # This is a test -/"]
  [str| "# This is a test"]

#eval runTest
  (title := "multi line sectioning comment")
  #[
    "/-! # This is a test",
    "of multiline section comment -/"
  ]
  [str|
    "# This is a test",
    "of multiline section comment"
  ]

#eval runTest
  (title := "empty lines")
  #["/-! test -/", "", "", ""]
  [str| "test"]

#eval runTest
  (title := "ignored lines")
  #["this is ignored --#", "this is also ignored --#"]
  [str| ""]

#eval runTest
  (title := "doc comment")
  #[
    "/-- This is a test -/",
    "def foo := 0"
  ]
  [str|
    "```lean",
    "/-- This is a test -/",
    "def foo := 0",
    "```"
  ]

#eval runTest
  (title := "multi line doc comment")
  #[
    "/-- This is a test",
    "of multiline doc comment -/",
    "def foo := 0"
  ]
  [str|
    "```lean",
    "/-- This is a test",
    "of multiline doc comment -/",
    "def foo := 0",
    "```"
  ]

#eval runTest
  (title := "block comment in doc comment")
  #[
    "/-- foo",
    "bar /- hoge -/",
    "baz -/"
  ]
  [str|
    "```lean",
    "/-- foo",
    "bar /- hoge -/",
    "baz -/",
    "```"
  ]

#eval runTest
  (title := "leading block comment in doc comment")
  #[
    "/-- foo",
    "/- hoge bar -/",
    "baz -/"
  ]
  [str|
    "```lean",
    "/-- foo",
    "/- hoge bar -/",
    "baz -/",
    "```"
  ]

#eval runTest
  (title := "convert doc comment syntax")
  #[
    "/-⋆-//-- doc comment -/",
    "def zero := 0"
  ]
  [str|
    "```lean",
    "/- doc comment -/",
    "def zero := 0",
    "```"
  ]

#eval runTest
  (title := "convert doc comment syntax - multiline")
  #[
    "/-⋆-//-- doc comment",
    "which is multiline -/",
    "def zero := 0"
  ]
  [str|
    "```lean",
    "/- doc comment",
    "which is multiline -/",
    "def zero := 0",
    "```"
  ]

#eval runTest
  (title := "convert doc comment syntax - indent")
  #[
    "namespace Foo",
    "  /-⋆-//-- doc comment -/",
    "  def zero := 0",
    "end Foo"
  ]
  [str|
    "```lean",
    "namespace Foo",
    "  /- doc comment -/",
    "  def zero := 0",
    "end Foo",
    "```"
  ]

#eval runTest
  (title := "multiple leading block comments in doc comment")
  #[
    "/-- foo",
    "/- hoge bar -/",
    "/- fuga -/ baz",
    "baz -/"
  ]
  [str|
    "```lean",
    "/-- foo",
    "/- hoge bar -/",
    "/- fuga -/ baz",
    "baz -/",
    "```"
  ]

#eval runTest
  (title := "block comment")
  #["/- this is a test -/"]
  [str| "this is a test"]

#eval runTest
  (title := "multi line block comment")
  #[
    "/-",
    "this is a test",
    "of multiline block comment -/"
  ]
  [str|
    "this is a test",
    "of multiline block comment"
  ]

#eval runTest
  (title := "respect indent")
  #[
    "hoge",
    "  fuga",
    "  monyo"
  ]
  [str|
    "```lean",
    "hoge",
    "  fuga",
    "  monyo",
    "```"
  ]

#eval runTest
  (title := "consecutive single-line block comments.")
  #[
    "/- hoge -/",
    "/- fuga -/"
  ]
  [str|
    "hoge",
    "",
    "fuga"
  ]

#eval runTest
  (title := "nested block comment")
  #[
    "/-",
    "this is a test",
    "/- nested comment -/",
    "of nested block comment -/"
  ]
  [str|
    "this is a test",
    "/- nested comment -/",
    "of nested block comment"
  ]

#eval runTest
  (title := "raw code block")
  #[
    "/-",
    "```lean",
    "/- this is test -/",
    "```",
    "fuga",
    "-/",
    "/- hoge -/"
  ]
  [str|
    "```lean",
    "/- this is test -/",
    "```",
    "fuga",
    "",
    "hoge"
  ]

#eval runTest
  (title := "indent in raw code block")
  #[
    "/-",
    "```lean",
    "  hoge",
    "```",
    "-/"
  ]
  [str|
    "```lean",
    "  hoge",
    "```"
  ]

#eval runTest
  (title := "doc comment in raw code block")
  #[
    "/-",
    "```lean",
    "/-- foo -/",
    "def zero := 0",
    "```",
    "-/"
  ]
  [str|
    "```lean",
    "/-- foo -/",
    "def zero := 0",
    "```"
  ]

#eval runTest
  (title := "multiple raw code blocks")
  #[
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
    "-/"
  ]
  [str|
    "```lean",
    "/-- greeting -/",
    "def foo := \"Hello World!\"",
    "```",
    "",
    "```lean",
    "/-! ### second code block -/",
    "",
    "def one := 1",
    "```"
  ]

#eval runTest
  (title := "lang metadata")
  #[
    "-- ⋆MDGEN_LANG⋆={#lst:id .lean caption=\"foo\"}",
    "def foo := 0"
  ]
  [str|
    "```{#lst:id .lean caption=\"foo\"}",
    "def foo := 0",
    "```"
  ]
