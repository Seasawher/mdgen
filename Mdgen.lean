import Mdgen.Cli


def main (args : List String) : IO UInt32 :=
  mkMdgenCmd.validate args
