```lean
variable (P Q R : Prop)

/-- multiline `sorry` -/
example : (P → Q) → (Q → R) → (P → R) := by
  sorry

/-- various size of indent -/
example : (P → Q) → (Q → R) → (P → R) := by
  try
    sorry

/-- focusing dot -/
example : (P → Q) → (P → R) → (P → Q ∧ R) := by
  intro pq pr p
  constructor
  · sorry
  · sorry

variable {n : Nat}

/-- inline `sorry` -/
example : 1 + n = n + 1 := calc
  1 + n = n + 1 := by sorry
  _ = n + 1 := by sorry
  _ = sorry := by rfl

/-- ignore pattern for a line -/
example : 1 + n = n + 1 := calc
  1 + n = 1 + n := by rfl
  _ = n + 1 := by sorry

/-- ignore pattern for a block -/
example : 1 + n = n + 1 := calc
  _ = 1 + n := by rfl
  _ = n + 1 := by ac_rfl
```
