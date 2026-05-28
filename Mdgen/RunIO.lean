module

public import Lean

/-
`run_io` is already defined in `Lake.DSL.Meta` but it is for `lakefile.lean` only.
We need to define it here for `Cli.lean` to use it.
-/

open Lean Meta Elab Command Term

set_option linter.missingDocs false

syntax:lead (name := runIO) "run_io " doSeq : term

public def toExprIO {α : Type} [ToExpr α] (x : IO α) : IO Expr :=
  toExpr <$> x

@[term_elab runIO]
public meta def elabRunIO : TermElab := fun stx expectedType? =>
  match stx with
  | `(run_io%$tk $t) => withRef t do
    let expectedType := mkApp (mkConst ``IO) <|
      expectedType?.getD <| ← mkFreshExprMVar <| mkSort <| .succ .zero
    let v ← elabTermEnsuringType (← `(do $t)) expectedType
    synthesizeSyntheticMVarsNoPostponing
    let v ← instantiateMVars v
    if (← logUnassignedUsingErrorInfos (← getMVars v)) then
      throwAbortTerm
    let v ← mkAppM ``toExprIO #[v]
    let io ← unsafe evalExpr (IO Expr) (mkApp (mkConst ``IO) (mkConst ``Expr)) v
    let (out, x) ← IO.FS.withIsolatedStreams io.toBaseIO
    unless out.isEmpty do
      logInfoAt tk out
    match x with
    | .ok x => return x
    | .error e => throwErrorAt tk e.toString
  | _ => Elab.throwUnsupportedSyntax
