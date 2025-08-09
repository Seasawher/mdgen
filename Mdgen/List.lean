/-- A new list created by removing the matching parts of two lists from the beginning. -/
def List.diff (as : List String) (bs : List String) : List String :=
  match as, bs with
  | [], [] => []
  | a, [] => a
  | [], _ => []
  | a :: as, b :: bs => if a == b then List.diff as bs else a :: as

#guard List.diff ["test", "src", "first"] ["test", "src"] = ["first"]

#guard List.diff ["test", "src", "first"] ["test", "out"] = ["src", "first"]

/-- a variant of `List.span` which return a list including
at most one "edge" element -/
def List.spanWithEdge {α : Type} (p : α → Bool) (as : List α) : List α × List α :=
  let ⟨l, r⟩ := as.span p
  match r with
  | [] => (l, [])
  | y :: ys => (l ++ [y], ys)
