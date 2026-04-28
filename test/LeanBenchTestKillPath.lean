import LeanBench

/-!
# Kill-path test: a slow benchmark, a tight cap, expect `killedAtCap`

Verifies the pure-Lean child-kill path end-to-end:

1. The parent calls `runOneBatch` with `maxSecondsPerCall := 0`,
   `killGraceMs := 100`, so the killer task fires almost immediately.
2. The benchmarked function loops long enough that one call takes
   well over 100 ms, so the child cannot finish and emit a row before
   the killer signals `Child.kill`.
3. The resulting `DataPoint` must have status `.killedAtCap`.

The same compiled binary plays both parent (no args) and child
(invoked via `_child` by `runOneBatch`'s spawn).
-/

/-- Deliberately-slow benchmark function. At `param = N` it performs
roughly `N × 1000` cheap UInt64 ops, which takes well over 100 ms
for the param this test uses. -/
def killPathSlowFn (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 0
  for _ in [0:n] do
    for _ in [0:1000] do
      x := x ^^^ n.toUInt64
  return x

setup_benchmark killPathSlowFn n => n

/-- Parent-side check: drive the kill path and assert the
synthesized row's status is `.killedAtCap`. -/
def parentRun : IO UInt32 := do
  let benchName := Lean.Name.mkSimple "killPathSlowFn"
  let some entry ← LeanBench.findRuntimeEntry benchName
    | do
        IO.eprintln "kill-path test: registry lookup failed"
        return 1
  let tightSpec : LeanBench.BenchmarkSpec :=
    { entry.spec with
      config :=
        { entry.spec.config with
          maxSecondsPerCall := 0
          killGraceMs := 100 } }
  let dp ← LeanBench.runOneBatch tightSpec 1_000_000
  match dp.status with
  | .killedAtCap =>
    IO.println "kill-path OK: child was killed at cap"
    return 0
  | s =>
    IO.eprintln s!"kill-path FAIL: expected killedAtCap, got {repr s}"
    return 1

def main (args : List String) : IO UInt32 := do
  match args with
  | "_child" :: _ => LeanBench.Cli.dispatch args
  | _ => parentRun
