# lean-bench

Microbenchmark library for Lean 4. Declare a benchmark, declare its
expected complexity, run it, and get back ratios `C = observed / expected`
that should be roughly constant if the implementation matches the
declared complexity.

```lean
import LeanBench

def goodFib : Nat → Nat
  | 0 => 0
  | 1 => 1
  | n + 2 => goodFib n + goodFib (n + 1)

setup_benchmark goodFib n => 2 ^ n
```

```bash
$ lake exe fib run goodFib
goodFib   complexity=2^n
  param  per-call         C
  0      … µs              —
  1      … µs              —
  4      … µs              0.94
  8      … µs              1.02
 16      … µs              1.01
 verdict: consistentWithDeclaredComplexity (cMin=0.94, cMax=1.02)
```

## Status

v0.1, Linux/macOS only. See [PLAN.md](PLAN.md) for the v0.2+ roadmap
and [doc/quickstart.md](doc/quickstart.md) for the user guide.

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
