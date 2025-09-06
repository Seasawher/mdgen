/-- This is a test for the markdown generator -/
def main : IO UInt32 := do
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

  return 0
