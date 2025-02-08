
/-- Syntax for String with break lines.
Note that trailing comma is not allowed -/
syntax "[str|" str,+ "]" : term

macro_rules
  | `([str| $str]) => `($str ++ "\n")
  | `([str| $str, $strs:str,*]) => `($str ++ "\n" ++ [str| $strs,*])

#guard
  let expected := "Hello, World!\nhoge\n"
  let actual := [str|
    "Hello, World!",
    "hoge"]
  expected = actual
