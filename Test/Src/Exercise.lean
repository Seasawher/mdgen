variable (P Q R : Prop)

/-- multiline `sorry` -/
example : (P → Q) → (Q → R) → (P → R) := by
  -- sorry
  intro pq qr pr
  exact qr (pq pr)
  -- sorry

/-- various size of indent -/
example : (P → Q) → (Q → R) → (P → R) := by
  try
    -- sorry
    intro pq qr pr
    exact qr (pq pr)
    -- sorry

/-- focusing dot -/
example : (P → Q) → (P → R) → (P → Q ∧ R) := by
  intro pq pr p
  constructor
  · -- sorry
    exact pq p
    done
    -- sorry
  · -- sorry
    exact pr p
    done
    -- sorry

variable {n : Nat}

/-- inline `sorry` -/
example : 1 + n = n + 1 := calc
  1 + n = n + 1 := by /- sorry -/ rw [Nat.add_comm]
  _ = n + 1 := by /- sorry -/ rfl
  _ = /-+-/ n + 1 /-+-/ := by rfl

/-- ignore pattern for a line -/
example : 1 + n = n + 1 := calc
  1 + n = 1 + n := by rfl
  _ = 1 + n := by rfl --##
  _ = 1 + n := by rfl --##
  _ = 1 + n := by rfl --##
  _ = 1 + n := by rfl --##
  _ = n + 1 := by /- sorry -/ rw [Nat.add_comm]

/-- ignore pattern for a block -/
example : 1 + n = n + 1 := calc
  _ = 1 + n := by rfl
  --##--
  _ = 1 + n := by rfl
  _ = 1 + n := by rfl
  _ = 1 + n := by rfl
  _ = 1 + n := by rfl
  --##--
  _ = n + 1 := by ac_rfl
