import Lake
open Lake DSL

package «mdgen» where
  -- add package configuration options here
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩
  ]

lean_lib «Mdgen» where
  -- add library configuration options here

@[default_target]
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

-- run `lake run test`
script test do
  if ← runCmd "lake" #["exe", "mdgen", "Test/Src", "Test/Out"] then return 1
  if ← runCmd "lean" #["--run", "Test.lean"] then return 1
  return 0
