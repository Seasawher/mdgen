import Mdgen.Cli

set_option linter.missingDocs false

def main (args : List String) : IO UInt32 :=
  mkMdgenCmd.validate args
