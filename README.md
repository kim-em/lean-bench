# lean-bench

Microbenchmark library for Lean 4. Declare a benchmark and the
complexity model you expect; run it; the harness reports per-call
times across an adaptive parameter ladder and the ratio
`C = perCallNanos / complexity(n)`. If your model fits, `C` is
roughly constant.

The ladder shape is auto-picked from the declared complexity:
doubling for polynomial growth, linear-inside-the-cap-bracket for
exponential.

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
-- Or with per-benchmark overrides on `BenchmarkConfig`:
-- setup_benchmark myFib n => n + 1 where {
--   maxSecondsPerCall := 0.5
--   paramCeiling := 4096
-- }

def main (args : List String) : IO UInt32 :=
  LeanBench.Cli.dispatch args
```

CLI flags layer on top of declared defaults, so you can tighten a
single run without recompiling:

```bash
$ lake exe my_benchmarks run myFib --max-seconds-per-call 0.25 --param-ceiling 1024
```

See [doc/quickstart.md](doc/quickstart.md#configuring-a-benchmark)
for the full flag list.

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

## Fixed-problem benchmarks

For workloads with a single canonical input — "this hard problem
takes ~1.2 s; the FLINT equivalent takes ~0.8 s" — there's a second
registration form with no parameter sweep:

```lean
def factorXOverFTwo : Polynomial Bool := …
setup_fixed_benchmark factorXOverFTwo
```

The runner does one warmup call followed by `repeats` measured calls
(default 5), reports median / min / max wall time, and hash-checks
across repeats. `run`, `compare`, `list`, and `verify` all dispatch
by registration kind. See [doc/quickstart.md](doc/quickstart.md#fixed-problem-benchmarks)
for the full guide.

## Status

v0.1. Cross-platform: works on every platform Lean's process API
supports (Linux, macOS, Windows). See [PLAN.md](PLAN.md) for the
v0.2+ roadmap, [doc/quickstart.md](doc/quickstart.md) for the
user guide, and [doc/schema.md](doc/schema.md) for the JSONL
result-row schema and its evolution rules.

## Design

Subprocess-per-batch. The child does the timing and the auto-tuned
inner-repeat loop; the parent spawns directly via `IO.Process.spawn`,
reads JSONL stdout, and kicks off a sibling `Task` that kills the
child via `IO.Process.Child.kill` if a single batch exceeds
`maxSecondsPerCall + killGraceMs`. No external `timeout(1)`
dependency. We use subprocesses because Lean has no portable
in-process interrupt for arbitrary computation, so the only reliable
way to enforce a wallclock cap is to give each measurement its own
process.

See [doc/design.md](doc/design.md) for the full architectural
rationale, including limits and known caveats.

## Caveats

Each measurement is a child process, so very fast operations have a
per-spawn noise floor in the milliseconds. The harness measures this
floor itself and prints it with every report.

The verdict is a thresholded log-log slope (`|β| ≤ 0.15` over the
trimmed tail), not a statistical test. β's sign tells you direction;
read the raw ratios for magnitude.

CI runs on Linux and macOS. Windows builds in CI but the test suite
isn't run there yet.
