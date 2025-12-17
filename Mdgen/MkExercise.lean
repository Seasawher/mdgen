module

import all Mdgen.String

/-- handle ignore pattern -/
private def filterIgnored (lines : Array String) : Array String := Id.run do
  let mut result := #[]
  let mut ignoredBlock := false
  for line in lines do
    if line.trimAscii.endsWith "--##" then
      continue

    -- ignore pattern for a block
    if line.trimAscii.endsWith "--##--" then
      ignoredBlock := ! ignoredBlock
      continue
    if ignoredBlock then
      continue

    result := result.push line
  return result

#guard
  let content := #[
    "hoge --##",
    "--##--",
    "fuga",
    "--##--",
    "piyo"
  ]
  filterIgnored content = #["piyo"]

/-- remove some content from the given content
and replace it with `sorry` -/
public def mkExercise (content : Array String) : Array String := Id.run do
  let content := filterIgnored content
    |>.map (fun line => line.replaceAfter "/- sorry -/" "sorry")
    |>.map (fun line => line.replaceEnclosed "/-+-/" "sorry")

  let mut listen := true
  let mut result : Array String := #[]
  for line in content do
    if let some sorry_idx := line.findWhere "-- sorry" then
      listen := ! listen
      if ! listen then
        result := result.push <| (line.take sorry_idx).copy ++ "sorry"
      continue

    if listen then
      result := result.push line
  return result

#guard
  let content := #[
    "def foo :=",
    "  -- sorry",
    "  hogehoge",
    "  -- sorry"
  ]
  let actual := mkExercise content
  let expected := #[
    "def foo :=",
    "  sorry",
  ]
  actual = expected
