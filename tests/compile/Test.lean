import Test.Enum
import Test.RawStruct

open Test

def allTests : IO Unit := do
  Enum.test
  RawStruct.test
