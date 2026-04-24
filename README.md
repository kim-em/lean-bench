# lean-bench

Microbenchmark library for Lean 4. Declare a benchmark, declare its
expected complexity, run it, and get back ratios `C = observed / expected`
that should be roughly constant if the implementation matches the
declared complexity.

## Try it

Clone, build, run a shipped example. No code to write:

```bash
$ git clone https://github.com/kim-em/lean-bench.git
$ cd lean-bench
$ lake build fib_benchmark_example
$ ./.lake/build/bin/fib_benchmark_example list
registered benchmarks:
  «LeanBench.Examples.Fib.goodFib»  →  complexity = «LeanBench.Examples.Fib.goodFib._leanBench_complexity»
  «LeanBench.Examples.Fib.badFib»   →  complexity = «LeanBench.Examples.Fib.badFib._leanBench_complexity»

$ ./.lake/build/bin/fib_benchmark_example run LeanBench.Examples.Fib.goodFib
…
       2 20.000000 ns ×16777216  C=5.142
       4 19.000000 ns ×16777216  C=1.248
       8 22.000000 ns ×16777216  C=0.344
      16 26.000000 ns ×16777216  C=0.103
       …
  524288  0.000000 ns       ×0  C=— [killed at cap]
  verdict: inconclusive (cMin=0.003, cMax=5.142)
  per-spawn floor (harness self-measurement): 24.7 ms
```

The verdict is `inconclusive` because the declared complexity `n*n`
isn't a great fit at small `n` — the raw ratios trace the boundary
where the bignum result outgrows one machine word.

`compare` is the same shape, side-by-side:

```bash
$ ./.lake/build/bin/fib_benchmark_example compare \
    LeanBench.Examples.Fib.goodFib LeanBench.Examples.Fib.badFib
…
common params (apples-to-apples): 0, 1, 2, 4, 8
agreement: all functions agree on common params
```

Two more examples, same shape:

```bash
$ lake build sort_benchmark_example binary_search_benchmark_example
$ ./.lake/build/bin/sort_benchmark_example list
$ ./.lake/build/bin/binary_search_benchmark_example list
```

## Use it in your project

```lean
import LeanBench

def goodFib (n : Nat) : Nat :=
  let rec go (k : Nat) (a b : Nat) : Nat :=
    if k = 0 then a else go (k - 1) b (a + b)
  go n 0 1

setup_benchmark goodFib n => n * n

def main (args : List String) : IO UInt32 :=
  LeanBench.Cli.dispatch args
```

Add this to your project's `lakefile.toml`:

```toml
[[require]]
name = "lean-bench"
git = "https://github.com/kim-em/lean-bench.git"
rev = "main"

[[lean_exe]]
name = "my_benchmarks"
root = "MyBenchmarks"
```

then `lake exe my_benchmarks list / run NAME / compare A B …`. The
[doc/quickstart.md](doc/quickstart.md) walks through more.

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

Subprocess-per-batch. The CHILD does the timing and the auto-tuned
inner-repeat loop; the PARENT only spawns, reads JSONL stdout, and
SIGTERMs the child if a single batch exceeds `--max-seconds-per-call`.
Why subprocess: Lean has no portable in-process interrupt for
arbitrary computation, so the only reliable way to enforce a wallclock
cap is to give each measurement its own process.

See [doc/design.md](doc/design.md) for the full architectural
rationale, including limits and known caveats.

## Caveats

Each measurement is a child process, so very fast operations have a
per-spawn noise floor in the milliseconds. The harness measures this
floor itself and prints it with every report.

The verdict is a thresholded ratio (`cMax/cMin ≤ 4`), not a
statistical test. Read the raw ratios.

Linux is tested. macOS/WSL probably work; Windows doesn't (see
Status).
