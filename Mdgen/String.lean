open String

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

/-- determines whether the string contains the given string and returns its first index -/
def String.findWhere (line tgt : String) : Option Nat := Id.run do
  for i in [0 : line.length - tgt.length + 1] do
    let rest := line.drop i
    if rest.startsWith tgt then
      return some i
  return none

#guard "hoge fuga".findWhere "fuga" = some 5
#guard "hoge fuga".findWhere "foo" = none
#guard "fuga fuga".findWhere "fuga" = some 0

/-- determines whether the string contains the given string and returns its all indexes -/
def String.findWhereAll (line tgt : String) : Array Nat := Id.run do
  let mut result := #[]
  for i in [0 : line.length - tgt.length + 1] do
    let rest := line.drop i
    if rest.startsWith tgt then
      result := result.push i
  return result

#guard String.findWhereAll "fuga fuga" "fuga" = #[0, 5]

/-- relace the part which is enclosed by `marker` with `tgt` -/
partial def String.replaceEnclosed (line marker tgt : String) : String := Id.run do
  let occurence := findWhereAll line marker

  if occurence.size = 0 then
    return line

  if occurence.size % 2 != 0 then
    panic! s!"invalid number of {marker} appear."

  let fst := occurence[0]!
  let snd := occurence[1]!
  return line.take fst ++ tgt ++ replaceEnclosed (line.drop (snd + marker.length)) marker tgt

#guard "/-+-/ hello /-+-/ world!".replaceEnclosed "/-+-/" "sorry" = "sorry world!"
#guard
  let line := "/-+-/ fuga /-+-/ greet /-+-/ hoge /-+-/"
  line.replaceEnclosed "/-+-/" "sorry" = "sorry greet sorry"

/-- replace everything after `tgt` with `rep` -/
def String.replaceAfter (line tgt rep : String) : String :=
  let index? := line.findWhere tgt
  match index? with
  | none => line
  | some index => line.take index ++ rep

#guard "def foo /- sorry -/ := hoge".replaceAfter "/- sorry -/" "sorry" = "def foo sorry"
