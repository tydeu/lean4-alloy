import S

#eval show IO _ from do
  getGlobalString >>= IO.println

#eval show IO _ from do
  IO.println (mkS 10 20 "hello").addXY
  IO.println (mkS 10 20 "hello").string
  updateGlobalS (mkS 0 0 "")
  appendToGlobalS "foo"
  appendToGlobalS "bla"
  getGlobalString >>= IO.println
  updateGlobalS (mkS 0 0 "world")
  getGlobalString >>= IO.println

#eval show IO _ from do
  getGlobalString >>= IO.println
