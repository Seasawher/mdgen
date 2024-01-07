/-- A new list created by removing the matching parts of two lists from the beginning. -/
def List.diff (as : List String) (bs : List String) : List String :=
  match as, bs with
  | [], [] => []
  | a, [] => a
  | [], _ => []
  | a :: as, b :: bs => if a == b then List.diff as bs else a :: as

example : List.diff ["test", "src", "first"] ["test", "out"] = ["src", "first"] := rfl
