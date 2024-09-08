import Mdgen.File
import Mdgen.ConvertToMd
import Cli

open Cli System

def runMdgenCmd (p : Parsed) : IO UInt32 := do
  let inputDir : FilePath := p.positionalArg! "input_dir" |>.as! String
  let outputDir : FilePath := p.positionalArg! "output_dir" |>.as! String

  let paths ← getLeanFilePaths inputDir

  for path in paths do
    let mut content ← IO.FS.lines path
    content := content.map (fun line => line.replace "\r" "")

    let outputFilePath := outputFilePath
      inputDir.components
      outputDir.components
      path.components

    createFile (genPath outputFilePath) (convertToMd content.toList)
  return 0

def mkMdgenCmd : Cmd := `[Cli|
  mdgen VIA runMdgenCmd; ["1.4.1"]
  "mdgen is a tool to generate .md files from .lean files."

  ARGS:
    input_dir : String; "The directory containing the input Lean files."
    output_dir : String; "The directory to write the markdown files."
]
