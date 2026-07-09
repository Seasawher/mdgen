import Lake
open Lake DSL

package «mdgen» where
  keywords := #["cli", "markdown"]
  description := "Tool to generate markdown files from lean files."

  -- add package configuration options here
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩,
    ⟨`linter.missingDocs, true⟩
  ]

@[default_target]
lean_lib «Mdgen» where
  -- add library configuration options here
  globs := #[.submodules `Mdgen]

require Cli from git
  "https://github.com/leanprover/lean4-cli.git" @ "main"

lean_exe «mdgen» where
  root := `Mdgen

def runCmdAux (input : String) : IO String := do
  let cmdList := input.splitOn " "
  let cmd := cmdList.head!
  let args := cmdList.tail |>.toArray
  let out ← IO.Process.output {
    cmd := cmd
    args := args
  }
  if out.exitCode != 0 then
    IO.println out.stderr
    throw <| IO.userError s!"Failed to execute: {input}"

  return out.stdout.trimAsciiEnd.copy

def runCmd (input : String) : IO Unit := do
  let out ← runCmdAux input
  if out != "" then
    IO.println out
  return ()

def checkVersion : IO Unit := do
  let expectedVer := s!"v{Lean.versionString}"
  let actualVer ← runCmdAux "lake exe mdgen --version"
  if actualVer != expectedVer then
    throw <| IO.userError
      s!"Version mismatch: expected {expectedVer}, got {actualVer}\n\
      Try this: update the mdgen CLI version string literal in Mdgen/Cli.lean to \"{expectedVer}\"."

/-- run test by `lake test` -/
@[test_driver] script test do
  checkVersion
  runCmd "lake exe mdgen Test/Src/Raw Test/Out"
  runCmd "lake exe mdgen --exercise Test/Src/Exercise.lean Test/Out/Exercise.md"
  runCmd "lake exe mdgen --copy Test/Src/Copy Test/Out"

  runCmd "lean --run Test.lean"
  return 0
