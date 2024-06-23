def main : IO UInt32 := do
  let testTargets := ["First.md", "Second/Second.md"]

  let mut failed := false
  for test_file in testTargets do
    let firstActual ← IO.FS.readFile ⟨s!"Test/Out/{test_file}"⟩
    let firstExpected ← IO.FS.readFile ⟨s!"Test/Exp/{test_file}"⟩
    if firstActual != firstExpected then
      IO.println s!"error: {test_file} is not as expected"
      failed := true

  return if failed then 1 else 0
