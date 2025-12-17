module

public import Mdgen.Cli

/-- the main entry point -/
public def main (args : List String) : IO UInt32 :=
  mkMdgenCmd.validate args
