import Mdgen.File

open System FilePath

#guard List.diff ["test", "src", "first"] ["test", "src"] = ["first"]

#guard List.diff ["test", "src", "first"] ["test", "out"] = ["src", "first"]

#guard outputFilePath ["."] ["out"] ["foo.lean"] = ofComponents ["out", "foo.md"]

#guard outputFilePath ["src"] ["."] ["src", "foo.lean"] = ofComponents ["foo.md"]

#guard outputFilePath ["src"] ["out"] ["src", "foo.lean"] = ofComponents ["out", "foo.md"]

#guard outputFilePath ["test", "src"] ["test", "out"] ["test", "src", "foo.lean"] = ofComponents ["test", "out", "foo.md"]

#guard outputFilePath ["src"] ["out", "dist"] ["src", "foo", "bar.lean"] = ofComponents ["out", "dist", "foo", "bar.md"]
