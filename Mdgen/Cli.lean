import Mdgen.File
import Mdgen.ConvertToMd
import Mdgen.MkExercise
import Cli
import Std
import Lean

open Cli System FilePath

/-- what `mdgen` does -/
def runMdgenCmd (p : Parsed) : IO UInt32 := do
  let inputDir : FilePath := p.positionalArg! "input_dir" |>.as! String
  let outputDir : FilePath := p.positionalArg! "output_dir" |>.as! String

  let paths ← getLeanFilePaths inputDir

  if p.hasFlag "count" then
    let mut globalCount := 0
    for path in paths do
      let content ← IO.FS.lines path
      let count := content.foldl (fun acc str => acc + str.length) 0
      globalCount := globalCount + count
    IO.println s!"The input Lean files contain a total of {globalCount} characters."

  for path in paths do
    let raw_content ← IO.FS.lines path

    let content :=
      if p.hasFlag "exercise" then
        -- replaces parts of the code with `sorry`
        mkExercise raw_content
      else
        raw_content

    let outputFilePath := outputFilePath
      inputDir.components
      outputDir.components
      path.components

    let newContent := convertToMd (some outputFilePath) (some outputDir) content
    createFile (path := outputFilePath) (content := newContent)
  return 0

/-- Get the release date of the project -/
def getReleaseDate : IO String := do
  let date ← Std.Time.PlainDate.now
  return date.format "uuuu-MM-dd"

/-- version message of `mdgen --version` -/
unsafe def versionMsg : String :=
  let dateExcept := unsafeIO getReleaseDate
  let date := match dateExcept with
    | Except.ok date => date
    | Except.error _ => "unknown date"
  s!"{Lean.versionString} (released on {date})"

/-- API definition of `mdgen` command -/
unsafe def mkMdgenCmd : Cmd := `[Cli|
  mdgen VIA runMdgenCmd; [versionMsg]
  "mdgen is a tool to generate .md files from .lean files."

  FLAGS:
    c, count; "Counts the total number of characters in the input Lean files. However, please be aware that the output may not be entirely accurate."
    e, exercise; "Erases parts of Lean code and replaces them with sorry."

  ARGS:
    input_dir : String; "The directory containing the input Lean files."
    output_dir : String; "The directory to write the markdown files."
]
