/-! # how to test

1. first, run `lake exe mdgen Test/Src Test/Out`
2. run `lake exe test`
-/

def main : IO UInt32 := do
  let firstActual ← IO.FS.readFile ⟨"Test/Out/first.md"⟩
  let firstExpected ← IO.FS.readFile ⟨"Test/Exp/first.md"⟩
  if firstActual != firstExpected then
    IO.eprintln "first.md is not as expected"
    return 1

  let secondActual ← IO.FS.readFile ⟨"Test/Out/Second/second.md"⟩
  let secondExpected ← IO.FS.readFile ⟨"Test/Exp/Second/second.md"⟩
  if secondActual != secondExpected then
    IO.eprintln "second.md is not as expected"
    return 1
  return 0
