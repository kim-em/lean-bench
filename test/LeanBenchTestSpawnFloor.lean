import LeanBench

/-!
# Spawn-floor self-measurement test

Pins the issue fixed here: `measureSpawnFloor` must not depend on the
presence or behavior of any registered benchmark. This test binary
imports only `LeanBench`, so the runtime registries should be empty.
-/

open LeanBench

def main : IO UInt32 := do
  let entries ← allRuntimeEntries
  let fixedEntries ← allFixedRuntimeEntries
  unless entries.isEmpty && fixedEntries.isEmpty do
    IO.eprintln s!"expected no registered benchmarks, got parametric={entries.size} fixed={fixedEntries.size}"
    return 1
  let env ← RunEnv.capture
  let floor? ← measureSpawnFloor env
  match floor? with
  | some n =>
    unless n > 0 do
      IO.eprintln s!"expected positive spawn floor, got {n}"
      return 1
  | none =>
    IO.eprintln "expected benchmark-independent spawn floor measurement, got none"
    return 1
  IO.println "spawn floor measurement is benchmark-independent"
  return 0
