# lean-bench

Microbenchmark library for Lean 4. Declare a benchmark and the
complexity model you expect; run it; the harness reports per-call
times across a doubling parameter ladder and the ratio
`C = perCallNanos / complexity(n)`. If your model fits, `C` is
roughly constant.

## Try it

From the repo root:

```
$ lake build && ./.lake/build/bin/fib_benchmark_example run LeanBench.Examples.Fib.goodFib
```

Builds the library + examples and runs a linear-Fibonacci benchmark
over params `0, 1, 2, 4, 8, …, 134_217_728`. Tail of the output:

```
       32_768  197.481 µs     ×2^9  C= 6.027
       65_536  374.301 µs     ×2^9  C= 5.711
      131_072  747.345 µs     ×2^8  C= 5.702
      262_144    1.496 ms     ×2^7  C= 5.708
      524_288    2.989 ms     ×2^6  C= 5.702
    1_048_576    5.979 ms     ×2^6  C= 5.702
    …
  verdict: consistent with declared complexity (cMin=5.702, cMax=6.863, β=-0.005)
```

Tombstones `†` on earlier rungs mark the warmup-trim region — those
rows aren't included in the slope fit.

`goodFib` is declared as `O(n)`; observed per-call time scales
linearly across 22 doublings (n=2 through n=134M); `C` stabilises
near 5.7 ns per iteration once the param is large enough that the
small fixed per-call cost amortises. `β` is the log-log slope of `C`
vs `param` over the trimmed tail — β ≈ 0 is what a correct complexity
declaration looks like.

## Use it in your project

```lean
import LeanBench

def myFib (n : Nat) : UInt64 := Id.run do
  let mut a : UInt64 := 0; let mut b : UInt64 := 1
  for _ in [0:n] do let c := a + b; a := b; b := c
  return a

setup_benchmark myFib n => n + 1

def main (args : List String) : IO UInt32 :=
  LeanBench.Cli.dispatch args
```

`lakefile.toml`:

```toml
[[require]]
name = "lean-bench"
git = "https://github.com/kim-em/lean-bench.git"
rev = "main"

[[lean_exe]]
name = "my_benchmarks"
root = "MyBenchmarks"
```

Then `lake exe my_benchmarks list / run NAME / compare A B / verify …`.
See [doc/quickstart.md](doc/quickstart.md).

## Status

v0.1, tested on Linux. See [PLAN.md](PLAN.md) for the v0.2+ roadmap
and [doc/quickstart.md](doc/quickstart.md) for the user guide.

The wallclock cap on each measurement is currently enforced by
shelling out to GNU coreutils `timeout(1)`. This works on any
platform with `timeout` in PATH (Linux out of the box; macOS if you
`brew install coreutils`; Windows under WSL). A pure-Lean
implementation using `IO.Process.Child.kill` would be cross-platform
and is on the v0.2 list — see [PLAN.md](PLAN.md).

## Design

Subprocess-per-batch. The child does the timing and the auto-tuned
inner-repeat loop; the parent only spawns, reads JSONL stdout, and
SIGTERMs the child if a single batch exceeds `--max-seconds-per-call`.
We use subprocesses because Lean has no portable in-process interrupt for
arbitrary computation, so the only reliable way to enforce a
wallclock cap is to give each measurement its own process.

See [doc/design.md](doc/design.md) for the full architectural
rationale, including limits and known caveats.

## Caveats

Each measurement is a child process, so very fast operations have a
per-spawn noise floor in the milliseconds. The harness measures this
floor itself and prints it with every report.

The verdict is a thresholded log-log slope (`|β| ≤ 0.15` over the
trimmed tail), not a statistical test. β's sign tells you direction;
read the raw ratios for magnitude.

Linux is tested. macOS/WSL probably work; Windows doesn't (see
Status).
