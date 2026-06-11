module

/-- This is test for generated markdown files -/
public def main : IO UInt32 := do
  let testTargets := [
    "First.md",
    "Second/Second.md",
    "Third/Third/Third.md",
    "Fourth.md",
    "Exercise.md"
  ]

  for test_file in testTargets do
    let actual ← IO.FS.readFile ⟨s!"Test/Out/{test_file}"⟩
    let expected ← IO.FS.readFile ⟨s!"Test/Exp/{test_file}"⟩
    if actual != expected then
      IO.println s!"error: {test_file} is not as expected"
      return 1

  let copiedTargets := [
    "test.svg",
    "assets/test.pdf"
  ]

  for test_file in copiedTargets do
    let actual ← IO.FS.readBinFile ⟨s!"Test/Out/{test_file}"⟩
    let expected ← IO.FS.readBinFile ⟨s!"Test/Src/Copy/{test_file}"⟩
    if actual != expected then
      IO.println s!"error: copied file {test_file} is not byte-identical"
      return 1

  return 0
