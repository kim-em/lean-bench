import BenchDocs.Block.Bench

/-! Test that the env-var gate behaves correctly: `LEAN_BENCH_DOCS_CHECK`
unset means the gate is closed; setting it to anything (including
`""` per `IO.getEnv`'s semantics) opens it. -/

def gateOpen : IO Bool := do
  return (← IO.getEnv "LEAN_BENCH_DOCS_CHECK").isSome

def main : IO UInt32 := do
  -- The exe is run twice by the test harness — once with the env var,
  -- once without — so this exe just reports its own gate observation.
  if (← gateOpen) then
    IO.println "gate: OPEN"
  else
    IO.println "gate: CLOSED"
  return 0
