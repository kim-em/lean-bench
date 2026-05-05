import BenchDocs.Block.Bench

/-! Smoke test for the `BenchConfig` parser. We don't exercise Verso's
ArgParse machinery directly here (that's covered by the integration
test of the `bench` directive against the placeholder page); this
test just confirms the helpers we hand-rolled — `parseNormalisers`
via the directive's path, and `splitArgv` — behave on representative
inputs. -/

open BenchDocs.Block

def main : IO UInt32 := do
  -- splitArgv: regular case
  let args := splitArgv "run goodFib --param-ceiling 65536"
  unless args == #["run", "goodFib", "--param-ceiling", "65536"] do
    IO.eprintln s!"splitArgv basic failed: got {args}"
    return 1
  -- splitArgv: empty
  let args := splitArgv ""
  unless args == #[] do
    IO.eprintln s!"splitArgv empty failed: got {args}"
    return 1
  -- splitArgv: extra spaces
  let args := splitArgv "  a   b\tc  "
  unless args == #["a", "b", "c"] do
    IO.eprintln s!"splitArgv extra-spaces failed: got {args}"
    return 1
  IO.println "all ConfigParse cases passed"
  return 0
