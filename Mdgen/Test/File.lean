import Mdgen.File

#guard List.diff ["test", "src", "first"] ["test", "src"] = ["first"]

#guard List.diff ["test", "src", "first"] ["test", "out"] = ["src", "first"]

#guard outputFilePath ["."] ["out"] ["foo.lean"] = ["out", "foo.md"]

#guard outputFilePath ["src"] ["."] ["src", "foo.lean"] = ["foo.md"]

#guard outputFilePath ["src"] ["out"] ["src", "foo.lean"] = ["out", "foo.md"]

#guard outputFilePath ["test", "src"] ["test", "out"] ["test", "src", "foo.lean"] = ["test", "out", "foo.md"]

#guard outputFilePath ["src"] ["out", "dist"] ["src", "foo", "bar.lean"] = ["out", "dist", "foo", "bar.md"]
