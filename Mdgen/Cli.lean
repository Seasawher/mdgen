module

import Mdgen.File
import Mdgen.ConvertToMd
import Mdgen.MkExercise
public meta import Mdgen.RunIO
public import Cli

open Cli System FilePath

/-- Spinner frames for displaying progress -/
def spinnerFrames : Array String :=
  #["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]

/-- Clears the current line in the terminal -/
def IO.clearLine : IO Unit := do
  IO.print "\r\x1b[2K"

/-- what `mdgen` does -/
public def runMdgenCmd (p : Parsed) : IO UInt32 := do
  let inputDir : FilePath := p.positionalArg! "input_dir" |>.as! String
  let outputDir : FilePath := p.positionalArg! "output_dir" |>.as! String

  let stdout ← IO.getStdout
  let paths ← getLeanFilePaths inputDir
  let mut characterCount := 0

  for (path, idx) in paths.zipIdx do
    let frame := spinnerFrames[idx % spinnerFrames.size]!
    stdout.putStr s!"\r{frame} Processing ..."
    stdout.flush

    let raw_content ← IO.FS.lines path

    if p.hasFlag "count" then
      let count := raw_content.foldl (fun acc str => acc + str.length) 0
      characterCount := characterCount + count

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

  IO.clearLine
  if p.hasFlag "count" then
    IO.println s!"The input Lean files contain a total of {characterCount} characters."
  IO.println "markedown files generated successfully!"
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
    c, count; "Counts the total number of characters in the input Lean files. However, please be aware that the output may not be entirely accurate."
    e, exercise; "Erases parts of Lean code and replaces them with sorry."

  ARGS:
    input_dir : String; "The directory containing the input Lean files."
    output_dir : String; "The directory to write the markdown files."
]
