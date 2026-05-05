import LeanBench

/-!
# Fixed-benchmark examples

Two fixed-problem benchmarks that record the absolute wall-clock
time to evaluate a single canonical input. These complement the
parametric ladder examples (`Fib`, `Sort`, `BinarySearch`): there is
no parameter sweep, no complexity model, and no verdict — the goal
is to record an absolute number to compare against external
references or to catch absolute-performance regressions.

- `tightFib1M`   — `Unit → IO UInt64`, the recommended shape.
- `tightFibIO1M` — `IO UInt64`, the legacy shape.

Both read the iteration count from an `IO.Ref` so the back-end
can't constant-fold the work away (issue #54).

Run them with:

```
lake exe fixed_benchmark_example list
lake exe fixed_benchmark_example run LeanBench.Examples.Fixed.tightFib1M
lake exe fixed_benchmark_example compare \
  LeanBench.Examples.Fixed.tightFib1M LeanBench.Examples.Fixed.tightFibIO1M
```
-/

namespace LeanBench.Examples.Fixed

/-- Linear Fibonacci, bottom-up, on `UInt64`. Identical to the `Fib`
    example's `goodFib`. Inlined here so this module has no
    cross-example dependency. -/
def goodFib (n : Nat) : UInt64 := Id.run do
  let mut a : UInt64 := 0
  let mut b : UInt64 := 1
  for _ in [0:n] do
    let c := a + b
    a := b
    b := c
  return a

/-- Iteration count read at runtime so the back-end can't fold
    `goodFib n` to a constant (issue #54). -/
initialize fibCountRef : IO.Ref Nat ← IO.mkRef 1_000_000

/-- Recommended shape: `Unit → IO α`. The `Unit` parameter forces
    re-invocation each repeat; the `IO.Ref` read defeats folding. -/
def tightFib1M : Unit → IO UInt64 := fun () => do
  return goodFib (← fibCountRef.get)

/-- Legacy shape: bare `IO α`. Same workload as `tightFib1M`. -/
def tightFibIO1M : IO UInt64 := do
  return goodFib (← fibCountRef.get)

setup_fixed_benchmark tightFib1M where {
  -- Pin the result hash so the run fails on a silent regression
  -- (issue #55). On the first run, copy the printed `observed hash:`
  -- value into the `where` clause.
  expectedHash := some (Hashable.hash (goodFib 1_000_000))
}
setup_fixed_benchmark tightFibIO1M where {
  -- Override declared defaults; the `repeats` knob has no parametric
  -- analogue, hence the dedicated fixed-only field on
  -- `FixedBenchmarkConfig`.
  repeats := 3
  maxSecondsPerCall := 5.0
}

end LeanBench.Examples.Fixed

def main (args : List String) : IO UInt32 :=
  LeanBench.Cli.dispatch args
