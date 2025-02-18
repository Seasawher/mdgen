import Lake
open Lake DSL

package «mdgen» where
  version := v!"1.11.0"
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

def IO.Process.Output.toString (self : IO.Process.Output) : String :=
  self.stdout.trimRight

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

  return out.toString

def runCmd (input : String) : IO Unit := do
  let _ ← runCmdAux input
  return ()

instance : ToString IO.Process.Output := ⟨IO.Process.Output.toString⟩

def checkVersion : IO Unit := do
  let out ← runCmdAux "lake exe mdgen --version"
  let cliVer := out
  let libVer := _package.version.toString
  if cliVer != libVer then
    IO.eprintln s!"Version mismatch: CLI {cliVer}, Library {libVer}"
    throw <| IO.userError "Version mismatch"

  let latest ← runCmdAux "git rev-list --tags --max-count=1"
  let latestVer ← runCmdAux s!"git describe --tags {latest}"

  if libVer != latestVer then
    IO.println s!"warning: latest release tag is {latestVer}, which is not the same as the library version {libVer}."


/-- run test by `lake test` -/
@[test_driver] script test do
  checkVersion
  runCmd "lake exe mdgen Test/Src Test/Out"
  runCmd "lean --run Test.lean"
  return 0
