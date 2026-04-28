import LeanBench

/-!
# Fixed-benchmark examples

Two fixed-problem benchmarks that record the absolute wall-clock
time to evaluate a single canonical input. These complement the
parametric ladder examples (`Fib`, `Sort`, `BinarySearch`): there is
no parameter sweep, no complexity model, and no verdict — the goal
is to record an absolute number to compare against external
references or to catch absolute-performance regressions.

- `tightFib60`  — `goodFib 60` as a pure value (returns `UInt64`).
   Demonstrates the bare `setup_fixed_benchmark` form on a pure
   computation.
- `tightFibIO60` — same workload wrapped in `IO`. Demonstrates the
   `setup_fixed_benchmark` form on an `IO α` value, useful when the
   benchmarked workload reads input from disk or shells out to an
   external tool.

Run them with:

```
lake exe fixed_benchmark_example list
lake exe fixed_benchmark_example run LeanBench.Examples.Fixed.tightFib60
lake exe fixed_benchmark_example compare \
  LeanBench.Examples.Fixed.tightFib60 LeanBench.Examples.Fixed.tightFibIO60
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

/-- A pure fixed benchmark: compute `goodFib 60` once, return the
    `UInt64` result. The benchmark name is the function name. -/
def tightFib60 : UInt64 := goodFib 60

/-- An `IO` fixed benchmark: same workload but inside `IO`. The
    `setup_fixed_benchmark` macro detects the `IO α` head shape and
    emits a runner that uses `←` to extract the value. -/
def tightFibIO60 : IO UInt64 := return goodFib 60

setup_fixed_benchmark tightFib60
setup_fixed_benchmark tightFibIO60 where {
  -- Override declared defaults; the `repeats` knob has no parametric
  -- analogue, hence the dedicated fixed-only field on
  -- `FixedBenchmarkConfig`.
  repeats := 3
  maxSecondsPerCall := 5.0
}

end LeanBench.Examples.Fixed

def main (args : List String) : IO UInt32 :=
  LeanBench.Cli.dispatch args
