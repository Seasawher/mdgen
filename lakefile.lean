import Lake
open Lake DSL

package «mdgen» where
  -- add package configuration options here

lean_lib «Mdgen» where
  -- add library configuration options here

@[default_target]
lean_exe «mdgen» where
  root := `Main
