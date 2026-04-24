# lean-bench

Microbenchmark library for Lean 4. Declare a benchmark, declare its
expected complexity, run it, and get back ratios `C = observed / expected`
that should be roughly constant if the implementation matches the
declared complexity.

```lean
import LeanBench

def goodFib (n : Nat) : Nat :=
  let rec go (k : Nat) (a b : Nat) : Nat :=
    if k = 0 then a else go (k - 1) b (a + b)
  go n 0 1

-- Lean's Nat is bignum, so each step's addition costs O(n) bit ops;
-- total is O(n²). See examples/Fib/Fib.lean for why the model isn't
-- just `n`.
setup_benchmark goodFib n => n * n
```

```bash
$ lake exe fib run goodFib
goodFib   complexity=goodFib._leanBench_complexity
  param   per-call    repeats  C
       2 20.000000 ns ×16777216  C=5.142
       4 19.000000 ns ×16777216  C=1.248
       8 22.000000 ns ×16777216  C=0.344
      16 26.000000 ns ×16777216  C=0.103
      …
  verdict: inconclusive (cMin=0.003, cMax=5.142)
```

The verdict is `inconclusive` here because `n * n` is too pessimistic
at small n (where the bignum is still one word and arithmetic is
constant-time). The raw ratios trace the boundary clearly. See
[doc/quickstart.md](doc/quickstart.md) for how to read them.

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

- **Process-spawn floor.** Each measurement spawns a child process.
  Module init for projects with heavy FFI or many imports can push
  the per-spawn floor into the tens of milliseconds. Total batch
  durations smaller than ~10× the empirical startup cost are noise,
  not algorithm data. Every result file's metadata records the
  measured per-spawn floor so you can sanity-check.
- **Verdict is heuristic.** `consistentWithDeclaredComplexity` is a
  weak label based on `cMax/cMin ≤ 4` over ≥3 data points past
  param=2. The raw ratios are the source of truth.
- **Cross-platform support is not yet verified.** Only Linux is
  tested. macOS/WSL likely work if `timeout(1)` is available.
  Native Windows is not currently supported (see Status above).
