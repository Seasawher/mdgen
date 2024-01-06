/- # sample of doc comment and ignoring -/

variable (n : Nat) --#

/-- adding zero changes nothing -/
theorem add_zero : n + 0 = n := by
  rw [Nat.add_zero]
