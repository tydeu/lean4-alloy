import Lake
open Lake DSL System

package alloy

lean_lib Alloy

@[defaultTarget]
lean_exe alloy {
  root := `Main
  supportInterpreter := true
}

--------------------------------------------------------------------------------

def runAlloy (path : FilePath) : LakeT IO PUnit := do
  let some alloyExe ← findLeanExe? &`alloy
    | error "no alloy executable configuration found in workspace"
  let alloy ← IO.Process.spawn {
    cmd := alloyExe.file.toString
    args := #[path.toString]
    env := #[("LEAN_PATH", (← getLeanPath).toString)]
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
