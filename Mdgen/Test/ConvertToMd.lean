import Mdgen.ConvertToMd

namespace analysis

def runTest (input : List String) (expected : List (Nat × Bool)) (title := "") : IO Unit :=
  let output := analysis input |>.map (fun x => (x.level, x.close))
  if output = expected then
    IO.println s!"{title} test passed!"
  else
    throw <| .userError s!"Test failed: \n{output}"

#eval runTest
  (title := "nested block comment")
  [
    "/-",
      "/- inline -/",
      "/- multi",
      "line -/",
      "hoge",
    "-/",
    "foo"
  ]
  [(1, false), (2, true), (2, false), (2, true), (1, false), (1, true), (0, false)]

#eval runTest
  (title := "sectioning comment and nested block comment")
  [
    "/-! hoge fuga",
      "/- foo! -/",
    "-/",
    "def foo := 1",
  ]
  [(1, false), (2, true), (1, true), (0, false)]

#eval runTest
  (title := "one line doc comment")
  [
    "/-- hoge -/",
    "def hoge := \"hoge\"",
  ]
  [(0, false), (0, false)]

#eval runTest
  (title := "multi line doc comment")
  [
    "/-- hoge",
    "fuga -/",
    "def hoge := 42",
  ]
  [(0, false), (0, false), (0, false)]

#eval runTest
  (title := "raw code block")
  [
    "/-",
      "```lean",
      "/-- greeting -/",
      "def foo := \"Hello World!\"",
      "```",
    "-/",
  ]
  [(1, false), (1, false), (1, false), (1, false), (1, false), (1, true)]

#eval runTest
  (title := "multi line ignoring")
  [
    "--#--",
    "this is ignored",
    "this is also ignored",
    "--#--",
    "hoge",
  ]
  [(0, false)]

end analysis


namespace ConvertToMd

def _root_.List.withBreakLine (as : List String) : String :=
  as.map (· ++ "\n") |>.foldl (· ++ ·) ""

def runTest (input : List String) (expected : List String) (title := "") : IO Unit :=
  let output := convertToMd input
  if output = expected.withBreakLine then
    IO.println s!"{title} test passed!"
  else
    throw <| .userError s!"Test failed: \n{output}"

#eval runTest
  (title := "inline comment")
  ["-- this is a test"]
  [
    "```lean",
    "-- this is a test",
    "```"
  ]

#eval runTest
  (title := "module document")
  ["/-! # This is a test -/"]
  ["# This is a test"]

#eval runTest
  (title := "multi line sectioning comment")
  [
    "/-! # This is a test",
    "of multiline section comment -/"
  ]
  [
    "# This is a test",
    "of multiline section comment"
  ]

#eval runTest
  (title := "empty lines")
  ["/-! test -/", "", "", ""]
  ["test"]

#eval runTest
  (title := "ignored lines")
  ["this is ignored --#", "this is also ignored --#"]
  [""]

#eval runTest
  (title := "doc comment")
  [
    "/-- This is a test -/",
    "def foo := 0"
  ]
  [
    "```lean",
    "/-- This is a test -/",
    "def foo := 0",
    "```"
  ]

#eval runTest
  (title := "multi line doc comment")
  [
    "/-- This is a test",
    "of multiline doc comment -/",
    "def foo := 0"
  ]
  [
    "```lean",
    "/-- This is a test",
    "of multiline doc comment -/",
    "def foo := 0",
    "```"
  ]

#eval runTest
  (title := "block comment")
  ["/- this is a test -/"]
  ["this is a test"]

#eval runTest
  (title := "multi line block comment")
  [
    "/-",
    "this is a test",
    "of multiline block comment -/",
  ]
  [
    "this is a test",
    "of multiline block comment"
  ]

#eval runTest
  (title := "respect indent")
  [
    "hoge",
    "  fuga",
    "  monyo",
  ]
  [
    "```lean",
    "hoge",
    "  fuga",
    "  monyo",
    "```"
  ]

#eval runTest
  (title := " consecutive single-line block comments.")
  [
    "/- hoge -/",
    "/- fuga -/",
  ]
  [
    "hoge",
    "",
    "fuga"
  ]

#eval runTest
  (title := "nested block comment")
  [
    "/-",
    "this is a test",
    "/- nested comment -/",
    "of nested block comment -/"
  ]
  [
    "this is a test",
    "/- nested comment -/",
    "of nested block comment"
  ]

#eval runTest
  (title := "raw code block")
  [
    "/-",
    "```lean",
    "/- this is test -/",
    "```",
    "fuga",
    "-/",
    "/- hoge -/",
  ]
  [
    "```lean",
    "/- this is test -/",
    "```",
    "fuga",
    "",
    "hoge"
  ]

#eval runTest
  (title := "indent in raw code block")
  [
    "/-",
    "```lean",
    "  hoge",
    "```",
    "-/"
  ]
  [
    "```lean",
    "  hoge",
    "```"
  ]

#eval runTest
  (title := "doc comment in raw code block")
  [
    "/-",
    "```lean",
    "/-- foo -/",
    "def zero := 0",
    "```",
    "-/",
  ]
  [
    "```lean",
    "/-- foo -/",
    "def zero := 0",
    "```"
  ]

#eval runTest
  (title := "multiple raw code blocks")
  [
    "/-",
    "```lean",
    "/-- greeting -/",
    "def foo := \"Hello World!\"",
    "```",
    "",
    "```lean",
    "/-! ### second code block -/",
    "",
    "def one := 1",
    "```",
    "-/",
  ]
  [
    "```lean",
    "/-- greeting -/",
    "def foo := \"Hello World!\"",
    "```",
    "",
    "```lean",
    "/-! ### second code block -/",
    "",
    "def one := 1",
    "```",
  ]

end ConvertToMd
