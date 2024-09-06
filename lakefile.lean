import Lake
open Lake DSL

package «mdgen» where
  -- add package configuration options here
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩
  ]

@[default_target]
lean_lib «Mdgen» where
  -- add library configuration options here
  globs := #[.submodules `Mdgen]

require Cli from git
  "https://github.com/leanprover/lean4-cli.git" @ "main"

lean_exe «mdgen» where
  root := `Mdgen

def runCmd (cmd : String) (args : Array String) : ScriptM Bool := do
  let out ← IO.Process.output {
    cmd := cmd
    args := args
  }
  let hasError := out.exitCode != 0
  if hasError then
    IO.eprint out.stderr
  return hasError

/-- run test by `lake test` -/
@[test_driver] script test do
  if ← runCmd "lake" #["exe", "mdgen", "Test/Src", "Test/Out"] then return 1
  if ← runCmd "lean" #["--run", "Test.lean"] then return 1
  return 0
