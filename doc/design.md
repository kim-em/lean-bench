# Design

Architectural rationale for the v0.1 cut. See [PLAN.md](../PLAN.md)
for v0.2+ planned features and [quickstart.md](quickstart.md) for
user-facing usage.

## Subprocess per measurement

Every measurement of `f` at a chosen `param` spawns a child process.
The child does the timing AND the auto-tuned-repeat loop; the parent
only spawns, reads stdout, and SIGTERMs (via `timeout(1)`) if the
batch exceeds `maxSecondsPerCall`.

Why subprocess: Lean 4 has no portable in-process interrupt for
arbitrary computation. `Task` cancellation is cooperative; `IO.sleep`
doesn't help. The only reliable way to enforce a wallclock cap on a
single measurement is to give it its own process and kill the
process.

Why `timeout(1)` rather than rolling our own SIGTERM/SIGKILL: the
coreutils utility already implements escalation cleanly, handles
process groups, and tracks exit status conventions (124 = timed out,
137 = SIGKILL, etc.). Less code, more reliable.

v0.1 has been tested only on Linux. macOS likely works if you have
GNU coreutils installed (the `timeout` binary). Native Windows is
not supported in v0.1 because it doesn't ship `timeout(1)`. None of
this is fundamental — `IO.Process.Child.kill` exists on every
platform Lean supports, so a pure-Lean cross-platform implementation
is just engineering. See PLAN.md F0.

## Why the timing AND repeat-doubling are in the child

A parent-side timer would include process spawn cost in the
measurement. Lean's startup includes module init, FFI library load,
and the runtime's own bookkeeping; that's tens of ms on a non-trivial
project. Including those in `total_nanos` would corrupt every
sub-millisecond measurement.

So the child does:

1. Run the function once. Time it.
2. While elapsed < `targetInnerNanos / 2`, double the inner repeat
   count and re-run from scratch.
3. Emit one JSONL row with `(inner_repeats, total_nanos, hash)`.

`per_call_nanos = total_nanos / inner_repeats`. The clock starts
*after* startup and stops *before* JSONL emission, so all the
parent-side / startup / IPC overhead is excluded.

## Dual registry (env extension + IO.Ref)

Two parallel registries, both populated by a single `LeanBench.register`
call emitted by the `setup_benchmark` macro:

- **Compile-time** (`SimplePersistentEnvExtension`): used by anything
  that needs to inspect the catalogue at elaboration / build time.
  Stored in the `.olean`; survives `import`.
- **Runtime** (`IO.Ref (Std.HashMap Name RuntimeEntry)`): used at
  runtime. Populated at module init via the `initialize` block the
  macro emits. The child looks up `runner` here for dispatch; the
  parent looks up `complexity` (to compute ratios) and `spec` (to
  drive the ladder).

The dual is unavoidable — env extensions are an elaboration-time
concept and the runtime needs callable closures — but the macro emits
exactly one `LeanBench.register` call so the duality stays out of
user code.

## Storage strategy

`setup_benchmark fib n => 2 ^ n` auto-generates two top-level defs:

- `fib._leanBench_complexity : Nat → Nat`
- `fib._leanBench_runChecked : Nat → IO (Option UInt64)`

The macro stores `Name`s pointing at these defs (not `Syntax`, not
`Expr`) in the env extension. Names are trivially serializable; the
defs themselves are normal Lean code and ship in the user's `.olean`.

`runChecked` is uniformly `Nat → IO (Option UInt64)`: returns `some
hash` when the function's return type has a `Hashable` instance, else
`none` after forcing the result. Uniform type avoids type-level
branching leaking through the design.

## What's intentionally NOT in v0.1

- **Elaboration-time subprocess sanity check** ("does `f 0` finish
  in < 1s?"). Codex review identified this as the worst design
  choice in the original draft: it makes the elaborator depend on
  the compiled binary, which is circular and phase-dependent. v0.1
  static checks only verify symbol existence, arity, and complexity
  type. Compiled-code sanity checks live in `lean-bench verify`,
  which spawns the existing child path against `f 0` and `f 1` for
  each registered benchmark and reports any non-`ok` rows.

- **Strong verdict labels.** The verdict fits the log-log slope β of
  `C` vs `param` over the trimmed tail (leading 20% of ratios
  dropped by default). `|β| ≤ 0.15` → "consistent with declared
  complexity", otherwise "inconclusive". β's sign is reported
  alongside the verdict so callers can see direction, but the verdict
  itself stays binary: the raw `ratios` array is the source of truth
  and the verdict is decoration. The labels deliberately don't say
  "matches" or "doesn't match" because the heuristic isn't defended
  statistically.

- **Statistical machinery.** No outlier rejection, confidence
  intervals, repeated outer trials. v0.1 trusts that
  `targetInnerNanos := 500ms` × auto-tuned inner-repeats is enough
  signal-to-noise for the orders of magnitude we care about.

- **Where-clause overrides on `setup_benchmark`.** v0.1 uses default
  `BenchmarkConfig` for every benchmark. `where { maxSecondsPerCall
  := 10 }` etc. is on the v0.2 list.

## Failure modes and synthesized rows

If the child exits 0 with a parseable JSONL row → use it. If the
child exits 124 (`timeout` graceful) or 137 (SIGKILL after
`--kill-after`) → synthesize a `killedAtCap` row. Other non-zero exits
get a synthesized `error` row. Both paths are explicitly tested in
`test/`.

## Ladder semantics

The ladder shape is per-benchmark and chosen via `BenchmarkConfig.paramSchedule`:

- `.doubling` — `[paramFloor, 1, 2, 4, …, paramCeiling]`. Stops at
  the first non-`ok` rung. Right for polynomial complexity: it
  spreads samples evenly in log-space, which is what the verdict's
  log-log slope fit needs.
- `.linear samples` — runs the doubling ladder as a *probe* (rows
  marked `partOfVerdict := false`), records the bracket
  `(lastOk, firstFail)`, then refines `firstFail` against the
  declared complexity model and the wallclock cap (so we don't
  burn the sweep budget on rungs guaranteed to time out), then
  walks `samples` consecutive rungs from `lastOk + 1` upward inside
  the refined bracket. Right for exponential complexity, where the
  cap kills doubling within 1–2 useful samples.
- `.auto` (default) — at runtime, evaluates the declared complexity
  at probe points 8/16/32/64 and resolves to one of the above. The
  test compares `complexity(64)/complexity(32)` against
  `complexity(16)/complexity(8)`: equal for any `n^k` (→ doubling),
  super-linearly larger for any `b^n` (→ linear, threshold 4×).

Probe rows in `.linear` mode are kept in the report (so the user
sees the cold-regime context that determined the bracket) but
filtered out of `cMin/cMax`/slope by `Stats.ratiosFromPoints`.

Two functions in the same `compare` may stop at very different
params; the report preserves both ladders fully and computes
`agreeOnCommon` only over the intersection.

### Verdict on narrow ladders

The slope-based verdict (`|β| ≤ slopeTolerance`) only works when the
log-x range is wide enough to make OLS well-conditioned. Linear
ladders span a narrow log-x range (e.g. `n ∈ 33..40` is `log
40/33 ≈ 0.19`), so `Stats.fitSlope` returns `none` for spans below
`1.0` and `deriveVerdict` falls back to a multiplicative range
check:

```
cMax / cMin  ≤  max(narrowRangeNoiseFloor, exp(slopeTolerance × xRange))
```

Two ingredients:

- The scale-adjusted slope bound `exp(slopeTolerance × xRange)`
  matches the slope semantics: it converges to `1 + slopeTolerance ·
  xRange` for small spans and admits exactly the constant-`C` data
  the slope check would. Naive `cMax/cMin ≤ 1 + slopeTolerance` is
  unsound (admits e.g. `n² log n` declared as `n²` because `log`
  varies slowly over any narrow interval).
- The `narrowRangeNoiseFloor` (default 1.50) admits the realistic
  15–25% spread of single-shot near-cap measurements. At the upper
  rungs of an exponential-complexity ladder the autotuner runs 1–4
  inner repeats per spawn (because each batch is already at the
  cap), so per-call timings are unsmoothed. Without a noise floor
  the bound `exp(slopeTolerance · xRange) ≈ 1.03` over `xRange ≈
  0.2` would reject the actual data run after run; `1.50` lets a
  flat-with-noise C pass with comfortable margin on noisy hardware
  while still flagging gross deviations.

The tradeoff: with a 1.50 noise floor on a narrow ladder, an actual
`n^(2.1)` declared as `n^2` over `n ∈ 33..40` would pass (cMax/cMin
≈ 1.20). On wider ladders that benchmark would use doubling and the
slope check would catch it. The noise floor only relaxes the
narrow-range path.
