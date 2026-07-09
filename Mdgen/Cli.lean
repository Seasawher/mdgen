module

import Mdgen.File
import Mdgen.ConvertToMd
import Mdgen.MkExercise
import Lean
public import Cli

open Cli System FilePath
open Lean Elab Term

syntax (name := mdgenCliVersion) "mdgenCliVersion% " str : term

/--
Term elaborator for `mdgenCliVersion%` that checks whether the CLI version string literal
matches the current Lean version and emits a TryThis replacement suggestion when it does not.
-/
@[term_elab mdgenCliVersion] def elabMdgenCliVersion : TermElab := fun stx expectedType? => do
  let versionStx := stx[1]
  let expectedVer := s!"v{Lean.versionString}"
  let actualVer := versionStx.getString
  if actualVer != expectedVer then
    Lean.Meta.Tactic.TryThis.addSuggestion
      (ref := versionStx)
      { suggestion := Syntax.mkStrLit expectedVer }
      (header := s!"mdgen CLI version mismatch: expected {expectedVer}, got {actualVer}")
  elabTerm versionStx expectedType?

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

/-- API definition of `mdgen` command -/
public def mkMdgenCmd : Cmd := `[Cli|
  mdgen VIA runMdgenCmd; [mdgenCliVersion% "v4.32.0-rc1"]
  "mdgen is a tool to generate .md files from .lean files."

  FLAGS:
    count; "Counts the total number of characters in the input Lean files."
    copy; "Files in inputDir other than .lean files are simply copied as-is into outputDir. But .md files are always ignored so that you can avoid conflicts."
    e, exercise; "Erases parts of Lean code and replaces them with sorry."

  ARGS:
    input_dir : String; "The directory containing the input Lean files."
    output_dir : String; "The directory to write the markdown files."
]
