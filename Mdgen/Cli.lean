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

  let allFiles ← getAllFilePaths inputDir
  let leanFiles := allFiles.filter isLeanFile
  let restFiles := allFiles.filter (fun fp => !isLeanFile fp && !isMarkdownFile fp)

  if p.hasFlag "count" then
    let mut globalCount := 0
    for leanFile in leanFiles do
      let content ← IO.FS.lines leanFile
      let countForFile := content.map String.length |>.sum
      globalCount := globalCount + countForFile
    IO.println s!"The input Lean files contain a total of {globalCount} characters."

  for leanFile in leanFiles do
    let rawContent ← IO.FS.lines leanFile

    let contentAfterRemovingSorry :=
      if p.hasFlag "exercise" then
        -- replaces parts of the code with `sorry`
        mkExercise rawContent
      else
        rawContent

    let outputFilePath := mkOutputFilePath inputDir outputDir leanFile

    let mdContent := convertToMd outputFilePath outputDir contentAfterRemovingSorry
    createFile (path := outputFilePath) (content := mdContent)

  if p.hasFlag "copy" then
    for restFile in restFiles do
      let outputFilePath := mkOutputFilePath inputDir outputDir restFile
      IO.FS.copyFile restFile outputFilePath
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
