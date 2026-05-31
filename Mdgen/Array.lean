module

variable {α : Type}

/-- Ensure `idx` is present in `indexes`. -/
public def Array.ensurePush [BEq α] (indexes : Array α) (idx : α) : Array α :=
  if indexes.contains idx then
    indexes
  else
    indexes.push idx
