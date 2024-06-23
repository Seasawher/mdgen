def main : IO UInt32 := do
  let testTargets := ["First.md", "Second/Second.md"]

  for test_file in testTargets do
    let firstActual ← IO.FS.readFile ⟨s!"Test/Out/{test_file}"⟩
    let firstExpected ← IO.FS.readFile ⟨s!"Test/Exp/{test_file}"⟩
    if firstActual != firstExpected then
      IO.eprintln s!"{test_file} is not as expected"
      return 1

  IO.println "All tests passed"
  return 0
