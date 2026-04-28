# lean-bench roadmap

v0.1 ships the minimum that makes the library useful: registration,
subprocess-isolated measurement, auto-tuned inner repeats, JSONL
output, single-benchmark run, comparison report, weak heuristic
verdict, and the `Fib` / `Sort` / `BinarySearch` examples.

This file enumerates the post-v0.1 features. None are required for
the library to be useful as-is. Order is rough priority.

## F1. Auto-fit complexity

Allow `setup_benchmark fib n` (no `=>`); the library tries fitting a
fixed catalogue
`[1, n, n * Nat.log2 n, n^2, n^3, 2^n]`
to the observed ratios and reports best fit. Useful when the user
doesn't want to commit to a model up front; complements the existing
"declare and verify" workflow.

Acceptance: `lake exe fib_benchmark_example run goodFib --auto-fit` reports
`best fit: n` for the linearised goodFib.

## F3. Memory metrics

Allocations and peak RSS via POSIX `getrusage(RUSAGE_CHILDREN)` after
the child exits. New optional `DataPoint` fields:
`allocBytes`, `peakRssKb`. Helps catch O(n) memory regressions when
runtime looks fine.

Acceptance: a benchmark whose function allocates a list proportional
to `n` shows `allocBytes` growing approximately linearly.

## F4. Cold vs warm cache

Config flag `coldCache : Bool := false`. When true, the parent
respawns the child for every individual measurement (no inner
repeats). When false (default), inner repeats happen as today.

Acceptance: a benchmark with non-trivial memory layout shows distinct
per-call times in cold vs warm mode.

## F5. Baseline-diff

`--baseline FILE` flag on the parent: read a prior JSONL run, print
regressions where the per-call time at a shared param exceeds the
baseline by more than a configurable threshold (default 10%).

Acceptance: rerun a benchmark with a deliberately-slower
implementation; baseline-diff flags the regression.

## F6. Output-hash divergence reports

When `compare` finds two functions disagree on `agreeOnCommon`, the
report includes a hex preview of each function's output (or first
N bytes if it can serialize) so the divergence is debuggable.

Acceptance: comparing `goodFib` and a deliberately-buggy `goodFib'`
emits a diff report that names both implementations and shows the
first-diverging param.

## F7. CI-budget mode

`--total-seconds 60` flag on the parent: schedule whole-suite runs
inside a wallclock budget, dropping families that don't fit and
tagging them with `status: "budget_skip"` in the report.

Acceptance: running with `--total-seconds 5` against a suite that
would naturally take 60s emits some completed families and some
`budget_skip` rows; total wall ≤ 5s + per-spawn floor.

## F8. `lake bench` Lake script

Add a `lake bench` script (one-liner) that invokes the user's bench
exe with sane defaults. Improves muscle memory; equivalent to
`lake exe bench`.

Acceptance: `lake bench` works identically to `lake exe <userExe>`
in a project that has imported lean-bench.

## F9. Hyperfine / json-export interop

`--export-hyperfine PATH` flag writing in the JSON format hyperfine
expects, so existing tooling (plotting, regression-analysis scripts)
can ingest results.

Acceptance: `lake exe fib_benchmark_example run goodFib --export-hyperfine out.json`
produces a file `hyperfine --export-json` could have produced.

---

## Explicitly dropped

**F2. Float-valued complexity model.** Considered during planning.
`Nat → Nat` plus `Nat.log2` (in core) covers `n log n`; users don't
need `Float`.

## Notes for contributors

The v0.1 architecture is documented in [doc/design.md](doc/design.md).
The retrospective from the predecessor harness (in `~/hex` benchmarks)
that motivated this library is at
[../hex/progress/2026-04-23T12:57:18Z_bench-prototype.md](../hex/progress/2026-04-23T12:57:18Z_bench-prototype.md)
and [../hex/TODO.md](../hex/TODO.md).
