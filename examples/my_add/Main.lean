import MyAdd

def main : IO Unit :=
  IO.println s!"According to compiled Alloy, 2 + 2 = {myAdd 2 2}"
