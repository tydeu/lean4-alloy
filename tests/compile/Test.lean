import Lib.Enum

def testEnum : IO Unit := do
  if myAdd .a .c = 8 then
    IO.println "enum test passed"
  else
    throw <| IO.userError "enum test failed"

def allTests : IO Unit := do
  testEnum
