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

  return out.stdout.trimRight

def runCmd (input : String) : IO Unit := do
  let _ ← runCmdAux input
  return ()

/-- check output of `mdgen --version` -/
def checkVersionString : IO Unit := do
  let version := s!"v{Lean.versionString}"
  let out ← runCmdAux "lake exe mdgen --version"
  if out != version then
    throw <| IO.userError s!"Version mismatch. expected {version}, got {out}"
  else
    IO.println s!"Version check passed: {out}"

/-- run test by `lake test` -/
@[test_driver] script test do
  checkVersionString
  runCmd "lake exe mdgen Test/Src Test/Out"
  runCmd "lake exe mdgen --exercise Test/SrcEx Test/Out"
  runCmd "lean --run Test.lean"
  return 0
