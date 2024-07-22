import Test.Enum
import Test.RawStruct
import Test.ScalarStruct

open Test

def allTests : IO Unit := do
  Enum.test
  RawStruct.test
  ScalarStruct.test
