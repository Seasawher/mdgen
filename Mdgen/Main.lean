import Mdgen.File
import Mdgen.ConvertToMd

open System

def main (args : List String) : IO UInt32 := do
  if args.length != 2 then
    IO.eprintln s!"usage: mdgen <input_dir> <output_dir>"
    return 1

  let inputDir : FilePath := args.get! 0
  let outputDir : FilePath := args.get! 1

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
