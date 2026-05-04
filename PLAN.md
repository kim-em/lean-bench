# lean-bench roadmap

This file enumerates future work. Anything already shipped has been
removed; consult the changelog or git log for history.

## F5. Baseline-diff

`--baseline FILE` flag on the parent: read a prior JSONL run, print
regressions where the per-call time at a shared param exceeds the
baseline by more than a configurable threshold (default 10%).

Acceptance: rerun a benchmark with a deliberately-slower
implementation; baseline-diff flags the regression.

## F6. Output-hash divergence: result previews

The hash-only side of divergence reporting landed in issue #7. What's
still missing is a *preview* of each function's actual output (first N
bytes / serialized prefix) so the divergence is fully debuggable
without re-running the function in a Lean file.

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

## Memory-metrics follow-on (issue #6)

The first iteration of memory metrics shipped with Linux-only
`peak_rss_kb` and a reserved-but-always-`null` `alloc_bytes`. Open
extensions:

- macOS / Windows peak-RSS probes (`getrusage` /
  `GetProcessMemoryInfo`) via FFI when there's a clear ask.
- Allocation counters once Lean exposes them in-process. Populating
  the field is a one-line change in `LeanBench.MemStats.capture`;
  readers downstream already handle the value.

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
