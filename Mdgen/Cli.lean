module

import Mdgen.File
import Mdgen.ConvertToMd
import Mdgen.MkExercise
public meta import Mdgen.RunIO
public import Cli

open Cli System FilePath

/-- what `mdgen` does -/
public def runMdgenCmd (p : Parsed) : IO UInt32 := do
  let inputDir : FilePath := p.positionalArg! "input_dir" |>.as! String
  let outputDir : FilePath := p.positionalArg! "output_dir" |>.as! String

  let paths ← getAllLeanFilePaths inputDir

  if p.hasFlag "count" then
    let mut globalCount := 0
    for path in paths do
      let content ← IO.FS.lines path
      let count := content.map String.length |>.sum
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

    let outputFilePath := mkOutputFilePath inputDir outputDir path

    let newContent := convertToMd (some outputFilePath) (some outputDir) content
    createFile (path := outputFilePath) (content := newContent)
  return 0

private def version : String := run_io do
  let fullVersion ← IO.FS.readFile "./lean-toolchain"
  let version := fullVersion.replace "leanprover/lean4:" ""
  return version

/-- API definition of `mdgen` command -/
public def mkMdgenCmd : Cmd := `[Cli|
  mdgen VIA runMdgenCmd; [version]
  "mdgen is a tool to generate .md files from .lean files."

  FLAGS:
    count; "Counts the total number of characters in the input Lean files."
    copy; "Files in inputDir other than .lean files are simply copied as-is into outputDir."
    e, exercise; "Erases parts of Lean code and replaces them with sorry."

  ARGS:
    input_dir : String; "The directory containing the input Lean files."
    output_dir : String; "The directory to write the markdown files."
]
