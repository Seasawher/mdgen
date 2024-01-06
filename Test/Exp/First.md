# Propositional Logic

```lean
variable (P Q : Prop)

example (hQ : Q) : P → Q := by
  intro _
  exact hQ
```

this is a very easy exercise.

```lean
example (h : P) : P ∨ Q := by
  apply Or.inl
  exact h
```
