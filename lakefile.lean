import Lake
open Lake DSL System

package alloy {
  supportInterpreter := true
}

def runAlloy (path : FilePath) : IO PUnit := do
  let alloyLib := FilePath.mk "build" / "lib"
  let alloyPath := FilePath.mk "build" / "bin" / "alloy"
  let alloy ← IO.Process.spawn {
    cmd := alloyPath.withExtension FilePath.exeExtension |>.toString
    args := #[path.toString]
    env := #[("LEAN_PATH", SearchPath.toString [alloyLib])]
  }
  let rc ← alloy.wait
  if rc ≠ 0 then
    IO.println s!"error: compiling {path} with alloy failed with exit code {rc}"

script examples do
  let exs: Array FilePath := #["my_add", "S"]
  for ex in exs do
    IO.println s!"compiling {ex} example ..."
    runAlloy <| "examples" / ex.withExtension "lean"
  return 0
