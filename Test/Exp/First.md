# What is Lean?
Lean is an *awesome* theorem prover.
Please see [the website](https://leanprover.github.io/) for more information.

## Propositional Logic
Lean can check mathematical proofs.

```lean
variable (P Q : Prop)

example (hQ : Q) : P → Q := by
  -- `_` is a proof of `P`, but we don't need it
  intro _
  exact hQ

/-- introducing `Or` -/
example (h : P) : P ∨ Q := by
  apply Or.inl
  exact h
```

## Function
Lean is
* a purely functional programming language
* an interactive theorem prover
You can implement an algorithm and prove its correctness at the same time.

```lean
def fib : Nat → Nat
  | 0 => 1
  | 1 => 1
  | n + 2 => fib n + fib (n + 1)

example : fib 10 = 89 := rfl

-- Why don't you jump in the Lean World?
```
