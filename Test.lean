/-! # how to test

1. first, run `lake exe mdgen Test/Src Test/Out`
2. run `lean --run Test.lean`
-/

def main : IO UInt32 := do
  let firstActual ← IO.FS.readFile ⟨"Test/Out/First.md"⟩
  let firstExpected ← IO.FS.readFile ⟨"Test/Exp/First.md"⟩
  if firstActual != firstExpected then
    IO.eprintln "First.md is not as expected"
    return 1

  let secondActual ← IO.FS.readFile ⟨"Test/Out/Second/Second.md"⟩
  let secondExpected ← IO.FS.readFile ⟨"Test/Exp/Second/Second.md"⟩
  if secondActual != secondExpected then
    IO.eprintln "Second.md is not as expected"
    return 1
  return 0
