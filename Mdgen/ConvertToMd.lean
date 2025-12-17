module

import all Mdgen.Analyze
import Mdgen.String
import all Mdgen.List
public import Mdgen.File

/-- block type in markdown -/
inductive TextType where
  /-- ground text -/
  | groundText
  /-- code block
  * `lang`: language metadata for markdown code block
  * `quoted`: whether the code block is quoted or not
  * `nestedCode`: whether the code block contains nested code block or not
  -/
  | codeBlock (lang : String) (quoted nestedCode : Bool)
deriving Repr

/-- A chunk of grouped code for conversion to markdown. -/
structure Block where
  /-- content of block -/
  content : String

  /-- additional info -/
  textType : TextType
deriving Repr

/-- Preprocess `RichLine` and extract metadata to be attached to code blocks -/
def RichLine.handleLangMeta (line : RichLine) : RichLine × Option String :=
  let token := "-- ⋆LANG⋆="
  if line.content.startsWith token then
    let lang := line.content.drop token.length |>.copy
    ({line with missing := true}, some lang)
  else
    (line, none)

/-- Preprocess `RichLine` and extract metadata for quotes -/
def RichLine.handleQuote (line : RichLine) : RichLine × Bool :=
  if line.content.startsWith "-- ⋆QUOTE⋆" then
    let newLine := { line with
      content := line.content
        |>.replace "-- ⋆QUOTE⋆," "--"
        |>.replace "-- ⋆QUOTE⋆" "--",
      missing := true
    }
    (newLine, true)
  else
    (line, false)

/-- build a first `Block` from a list of `RichLine`s.
This returns the first `Block` and the remaining `RichLine`s. -/
def buildFstBlock (lines : List RichLine) : Block × List RichLine :=
  let (line, quoted) := lines.head!.handleQuote
  let (line, lang?) := line.handleLangMeta
  let lines := line :: lines.tail!

  let ⟨_, level, _, _⟩ := line
  let splited := (
    if level == 0 then
      lines.span (fun x => x.level == 0)
    else
      lines.spanWithEdge (fun x => x.level > 1 || ! x.close)
  )
  let isCodeBlock := level == 0
  let nestedCode := splited.fst
    |>.map (fun rline => rline.content)
    |>.any (fun line => line.trimAsciiStart.startsWith "```")
  let fstBlock : Block := {
    content := splited.fst
      |>.dropWhile (fun line => line.missing)
      |>.map (fun line => line.content ++ "\n")
      |>.map (fun raw_line => if quoted then "> " ++ raw_line else raw_line)
      |>.foldl (· ++ ·) ""
      |>.trimAscii
      |>.copy,
    textType :=
      if isCodeBlock then
        TextType.codeBlock (lang?.getD "lean") quoted nestedCode
      else
        TextType.groundText
  }
  (fstBlock, splited.snd)

/-- build a `Block` from a `RichLine` -/
partial def buildBlocks (lines : List RichLine) : List Block :=
  buildBlocksAux [] lines
where
  /-- Tail-recursive worker for `buildBlocks`. -/
  buildBlocksAux (acc : List Block) (lines : List RichLine) : List Block :=
    match lines with
    | [] => acc.reverse
    | _ =>
      let (block, rest) := buildFstBlock lines
      buildBlocksAux (block :: acc) rest

/-- convert a `Block` intro a markdown snippet -/
protected def Block.toString (b : Block) : String := Id.run do
  if b.content == "" then
    return "\n"

  match b.textType with
  | .codeBlock lang quoted nestedCode =>
    let codeBlockMarker := if nestedCode then "````" else "```"
    match quoted with
    | false => s!"\n{codeBlockMarker}{lang}\n{b.content}\n{codeBlockMarker}\n\n"
    | true => s!"> {codeBlockMarker}{lang}\n{b.content}\n> {codeBlockMarker}\n"
  | .groundText =>
    let separator := if b.content.startsWith "/-!" then "/-!" else "/-"
    b.content
      |> (String.drop · separator.length)
      |> (String.Slice.dropEnd · "-/".length)
      |> String.Slice.trimAscii
      |>.copy
      |> (· ++ "\n")

/-- merge blocks and build a markdown content -/
def mergeBlocks (blocks : List Block) : String :=
  let res := blocks
    |>.map Block.toString
    |>.foldl (· ++ ·) ""
  res.trimAscii.copy ++ "\n"

open System FilePath

/-- Handle uniform internal link syntax.
This converts `#{root}` in internal link to repeated `../` string -/
def Block.handleUILStx (outputFilePath outputDir : FilePath) (b : Block) : Block := Id.run do
  match b.textType with
  | .codeBlock _ _ _ =>
    return b
  | .groundText =>
    let pathPrefix := relativePath outputFilePath outputDir
      |>.drop 1
      |>.map (· ++ "/")
      |>.foldl (· ++ ·) ""
    let newContent := b.content
      |>.replace "#{root}/" pathPrefix
    return {b with content := newContent}

/-- convert lean contents to markdown contents. -/
public def convertToMd (outputFilePath outputDir : Option FilePath := none) (lines : Array String) : String :=
  let blocks := buildBlocks <| analyze lines

  let postProcessedBlocks :=
    match outputFilePath, outputDir with
    | some outputFilePath, some outputDir =>
      blocks.map (Block.handleUILStx outputFilePath outputDir)
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
    "fuga"
  ]

#eval runTest
  (title := "insert empty line")
  #[
    "/- hoge -/",
    "",
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
    "-- ⋆LANG⋆={#lst:id .lean caption=\"foo\"}",
    "def foo := 0"
  ]
  [str|
    "```{#lst:id .lean caption=\"foo\"}",
    "def foo := 0",
    "```"
  ]

#eval runTest
  (title := "quoted code")
  #[
    "-- ⋆QUOTE⋆",
    "def foo := 42",
    "",
    "def hoge := 22",
  ]
  [str|
    "> ```lean",
    "> def foo := 42",
    "> ",
    "> def hoge := 22",
    "> ```"
  ]

#eval runTest
  (title := "quoted code insertion")
  #[
    "/-",
    "> hoge",
    "-/",
    "-- ⋆QUOTE⋆",
    "def foo := 42",
    "/-",
    "> foo is foo",
    "-/",
  ]
  [str|
    "> hoge",
    "> ```lean",
    "> def foo := 42",
    "> ```",
    "> foo is foo"
  ]

#eval
  runTest
  (title := "lang metadata with quoted code")
  #[
    "/-",
    "> hoge",
    "-/",
    "-- ⋆QUOTE⋆ ⋆LANG⋆=hoge",
    "def foo := 42",
    "/-",
    "> foo is foo",
    "-/",
  ]
  [str|
    "> hoge",
    "> ```hoge",
    "> def foo := 42",
    "> ```",
    "> foo is foo"
  ]

#eval
  runTest
  (title := "lang metadata with quoted code (comma syntax)")
  #[
    "/-",
    "> hoge",
    "-/",
    "-- ⋆QUOTE⋆, ⋆LANG⋆=hoge",
    "def foo := 42",
    "/-",
    "> foo is foo",
    "-/",
  ]
  [str|
    "> hoge",
    "> ```hoge",
    "> def foo := 42",
    "> ```",
    "> foo is foo"
  ]

#eval
  runTest
  (title := "code block in doc comment")
  #[
    "/--",
    "```lean",
    "def foo := 42",
    "```",
    "-/",
    "def bar := 22",
  ]
  [str|
    "````lean",
    "/--",
    "```lean",
    "def foo := 42",
    "```",
    "-/",
    "def bar := 22",
    "````"
  ]
