open System

/-- A new list created by removing the matching parts of two lists from the beginning. -/
private def List.diff (as : List String) (bs : List String) : List String :=
  match as, bs with
  | [], [] => []
  | a, [] => a
  | [], _ => []
  | a :: as, b :: bs => if a == b then List.diff as bs else a :: as

#guard List.diff ["test", "src", "first"] ["test", "src"] = ["first"]

#guard List.diff ["test", "src", "first"] ["test", "out"] = ["src", "first"]

/-- a function which returns an output file path component
given components of input and output directories. -/
def outputFilePath (inputDir : List String) (outputDir : List String)
    (path : List String) : List String :=
  let relativePath := path.diff inputDir
  outputDir ++ relativePath
    |> List.map (String.replace · ".lean" ".md")
    |> List.filter (· ≠ ".")

#guard outputFilePath ["."] ["out"] ["foo.lean"] = ["out", "foo.md"]

#guard outputFilePath ["src"] ["."] ["src", "foo.lean"] = ["foo.md"]

#guard outputFilePath ["src"] ["out"] ["src", "foo.lean"] = ["out", "foo.md"]

#guard outputFilePath ["test", "src"] ["test", "out"] ["test", "src", "foo.lean"] = ["test", "out", "foo.md"]

#guard outputFilePath ["src"] ["out", "dist"] ["src", "foo", "bar.lean"] = ["out", "dist", "foo", "bar.md"]

/-- generate a filepath from path components -/
def genPath (l : List String) : FilePath :=
  l |> List.map (· ++ FilePath.pathSeparator.toString)
    |> List.foldl (· ++ ·) ""
    |> (String.dropRight · 1)
    |> FilePath.mk
