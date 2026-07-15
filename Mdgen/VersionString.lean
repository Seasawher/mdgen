module

public import Lean

open Lean Elab Term

/-- version string literal with automatic checking -/
syntax (name := mdgenCliVersion) "version% " str : term

/--
Term elaborator for `version%` that checks whether the CLI version string literal
matches the current Lean version and emits a TryThis replacement suggestion when it does not.
-/
@[term_elab mdgenCliVersion]
public meta def mdgenCliVersionElab : TermElab := fun stx expectedType? => do
  let versionStx ←
    match stx with
    | `(version% $ver:str) => pure ver
    | _ => throwUnsupportedSyntax
  let expectedVer := s!"v{Lean.versionString}"
  let actualVer := versionStx.getString
  if actualVer != expectedVer then
    Lean.Meta.Tactic.TryThis.addSuggestion
      (ref := versionStx)
      { suggestion := Syntax.mkStrLit expectedVer }
      (header := s!"Version mismatch: expected {expectedVer}, got {actualVer}")
  elabTerm versionStx expectedType?
