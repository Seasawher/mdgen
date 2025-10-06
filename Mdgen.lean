import Mdgen.Cli

/-- the main entry point -/
def main (args : List String) : IO UInt32 :=
  mkMdgenCmd.validate args
