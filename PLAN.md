# lean-bench roadmap

v0.1 ships the minimum that makes the library useful: registration,
subprocess-isolated measurement, auto-tuned inner repeats, JSONL
output, single-benchmark run, comparison report, weak heuristic
verdict, and the `Fib` / `Sort` / `BinarySearch` examples.

This file enumerates the post-v0.1 features. None are required for
the library to be useful as-is. Order is rough priority.

## F1. Auto-fit complexity (shipped — issue #8)

Implemented as a `--auto-fit` flag on `run` / `compare`. After the
standard verdict block, the harness fits the fixed catalogue
`[1, n, n * Nat.log2 (n + 1), n^2, n^3, 2^n]` against the observed
per-call timings and prints a ranked suggestion (lowest stddev of
`log C` wins). The declared-model verdict is unaffected — auto-fit
is purely advisory. See
[`doc/quickstart.md#auto-fit`](doc/quickstart.md#auto-fit) for the
catalog, the scoring method, and limits.

The originally-proposed declaration-time form (`setup_benchmark fib n`
with no `=>`) is dropped: a runtime flag matches the use case better
(quickly probe a function whose model is unclear) without requiring
the user to commit to a placeholder declaration. The acceptance
example still works:
`lake exe fib_benchmark_example run goodFib --auto-fit` reports `n`
as the best fit.

## F3. Memory metrics (shipped — issue #6)

`DataPoint` and `FixedDataPoint` carry optional `allocBytes` /
`peakRssKb` fields, the JSONL wire format gained matching
`alloc_bytes` / `peak_rss_kb` keys (additive, no schema bump),
the export format includes them as first-class per-point fields,
and the human-readable report surfaces a per-result memory line
when at least one row carries a measurement.

The first iteration captures `peak_rss_kb` on Linux only, by reading
`/proc/self/status`'s `VmHWM:` line at the end of the child's batch.
macOS / Windows / Emscripten emit `null` for the field; the
[schema doc](doc/schema.md#memory-metrics) documents the platform
support and the interpretation rules (the value is the child's peak
including the Lean runtime's own footprint, most useful as a delta
across runs of the same benchmark).

`alloc_bytes` is reserved in the schema but always emitted as `null`
today — Lean 4 has no portable in-process API for total bytes
allocated, and the issue called out the field as "if there is a
reliable way to obtain them." When a future Lean release exposes
allocation counters in-process, populating the field is a one-line
change in `LeanBench.MemStats.capture`; readers downstream already
handle the value (parsers, exporters, formatters).

Future work in this slot:

- macOS / Windows peak-RSS probes (`getrusage` /
  `GetProcessMemoryInfo`) via FFI when there's a clear ask.
- Allocation counters once Lean exposes them in-process.

## F4. Cold vs warm cache (shipped — issue #12)

Implemented as a `cacheMode : CacheMode` config field (`.warm` /
`.cold`, default `.warm`) exposed via
`setup_benchmark … where { cacheMode := .cold }` at declaration time
and `--cache-mode warm|cold` on `run` / `compare` at run time. Cold
mode skips the in-child auto-tuner: the child runs the function
exactly once per spawn (`inner_repeats := 1`) and the parent
respawns the child for every ladder rung, so cache state is not
preserved across measurements. The emitted JSONL row carries
`cache_mode: "warm" | "cold"` and the result table tags each block
with `[warm cache]` / `[cold cache]`. See
[`doc/advanced.md#cache-modes`](doc/advanced.md#cache-modes) for the
"what is being measured?" framing.

## F5. Baseline-diff

`--baseline FILE` flag on the parent: read a prior JSONL run, print
regressions where the per-call time at a shared param exceeds the
baseline by more than a configurable threshold (default 10%).

Acceptance: rerun a benchmark with a deliberately-slower
implementation; baseline-diff flags the regression.

## F6. Output-hash divergence reports

The hash-only side landed in issue #7: `compare` now reports the
earliest diverging param, names the implementations involved, lays
the per-function hash table side-by-side, and tags each dissenter
with the baseline it disagrees with. What's still missing is a
*preview* of each function's actual output (first N bytes /
serialized prefix) so the divergence is fully debuggable without
re-running the function in a Lean file.

Plan: add an optional `result_preview : string | null` field to the
JSONL row (additive — no schema bump needed; see
[`doc/schema.md`](doc/schema.md)), populate it from `setup_benchmark`
when `ToString α` synthesizes (truncated to ~80 characters), and
extend the formatter to surface the preview alongside the hash on
the divergence line.

Acceptance: comparing `goodFib` and a deliberately-buggy `goodFib'`
emits a divergence report whose earliest-divergence block carries a
visible string preview of each implementation's return value at
that param, in addition to the hash.

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
