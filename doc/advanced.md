# Advanced

Beyond the basics in [quickstart.md](quickstart.md). All of this is
v0.1; v0.2+ items live in [PLAN.md](../PLAN.md).

## Comparing implementations

`lake exe bench compare A B C [...]` runs each named benchmark
through its own (auto-picked) ladder, then prints:

- the full per-function table (one block per benchmark)
- the `commonParams` intersection (the params at which every
  function produced an `ok` row)
- `agreement` over those params: all agreed, diverged at specific
  params, or hash unavailable (when at least one function's return
  type lacks `Hashable`)

A function whose ladder stops early (because its complexity rises
faster) doesn't break the comparison — its rows are preserved and
the agreement check just skips the params it didn't reach.

The Hashable opt-in: `setup_benchmark` synthesizes `Hashable α`
once at registration. If it succeeds, every measurement records the
hash and `compare` checks them. If not, `runChecked` still forces
the result (so the benchmark measures real work) but writes
`result_hash := none`, and `compare` reports `hashUnavailable` and
names the offending registration so you know which return type to
fix.

### Reading a divergence report

When `compare` finds disagreement, the summary highlights the
**earliest** common param at which any pair of implementations
produced different output, lays the per-function hash table out
side-by-side, and tags each non-baseline implementation with the
baseline it disagrees with. The first listed implementation
(`compare A B C` → `A`) is the comparison baseline. Example:

```
common params (apples-to-apples): 2, 4, 8
agreement: DIVERGED on 2 params — earliest divergence at param=4:
    Sample.linearFn  hash=0xabc    (baseline)
    Sample.otherFn   hash=0xdef    differs from Sample.linearFn
  also diverged at: 8
  (only result hashes are available; see doc/quickstart.md for what to expect)
```

Hashes are reported as hex literals. The `also diverged at:` line
lists every later common param where any pair still disagreed —
useful for telling "one off-by-one bug at small n" from "the two
implementations are entirely different functions". Result *previews*
(short string snapshots of the actual return values) are not
recorded today — the wire format only carries hashes, and a 64-bit
hash gives no way to recover the input. See
[quickstart.md](quickstart.md#what-compare-shows-on-divergence) for
the recommended debugging workflow when only hashes are available.

## Reading the verdict

The raw `ratios` array `[(param, perCallNanos / complexity(param)), …]`
is the source of truth. The verdict is a heuristic decoration.

The decision rule is the log-log slope β of `C` vs `param` over the
trimmed tail (the leading `verdictWarmupFraction` of ratios — 20% by
default — is dropped first, so the cold regime can't corrupt the fit):

- "consistent with declared complexity" if `|β| ≤ slopeTolerance`
  (0.15 by default)
- "inconclusive" otherwise

β is printed on the verdict line alongside `cMin`/`cMax`. A correct
complexity declaration gives β ≈ 0; positive β means the function
grows faster than declared by roughly a factor of `n^β`, negative β
means slower. The verdict line surfaces a "looks slower/faster than
declared by ~n^β" hint whenever `|β|` is both outside tolerance and
at least 0.05.

An inconclusive verdict is not "the implementation is wrong" — it
just means the model + measurement noise doesn't support the strong
claim. Things that produce an inconclusive verdict:

- declared complexity is wrong (e.g. `n` for an O(n²) algorithm — β
  comes back near 1)
- declared complexity is too coarse (e.g. `2^n` for an algorithm
  whose actual cost is `φ^n` — β comes back negative; the deviation
  is exponential, not polynomial, so the "~n^β" shorthand is a
  rough summary rather than an exact exponent)
- cache effects (smooth at small n, jumps at L1 / L2 boundaries)
- arbitrary-precision arithmetic on growing results (Lean's `Nat`
  is bignum; if `f n` returns a number with k bits, every operation
  on it costs O(k))

For exponential complexity, the verdict's β line shows `—` because
the slope fit is rejected for narrow log-x ranges (the fit would be
ill-conditioned). The `cMin/cMax` range check takes over: the
verdict is consistent iff `cMax/cMin ≤ max(narrowRangeNoiseFloor,
exp(slopeTolerance · xRange))`. Default `narrowRangeNoiseFloor = 1.50`
admits the 15-25% spread that single-shot near-cap measurements show
on noisy hardware. Tighten if you want to discriminate finer model
differences and accept the false-negative rate; widen further on
chronically noisier hardware.

β tells you the direction; the raw ratios tell you the magnitude.
Together they're enough to tell "off by a polynomial factor" from
"off by a constant factor but drifting" from "just noisy".

## The per-spawn floor

Every result includes
`per-spawn floor (harness self-measurement): X ms`. This is the
harness's own startup cost, measured at the start of each run by
spawning a no-op child against the first registered benchmark.

Any data point whose `total_nanos` is smaller than ~10× the floor is
noise. The auto-tuner inside the child usually drives the inner-repeat
count up enough that this isn't a problem (each batch targets ~500ms,
which is much larger than typical floors), but for very fast
operations on small `param` you may see flat or non-monotone per-call
times until the param grows past where startup dominates.

## What this library is not

- A statistical benchmarking framework. No CIs, no t-tests, no
  outlier rejection. v0.1 uses one batch per param and trusts the
  auto-tuner.
- A profiler. Per-call time is wall time; we don't break it down
  by function or measure allocations. (Allocation tracking is on
  the v0.2 roadmap, F3.)
- A measure of compile-time, kernel-reduction-time, or `#eval` time.
  Only compiled-code wall time. (Per the SPEC v2 discussion in
  `~/hex/TODO.md`.)

## CI integration

A typical CI step runs the bench exe with a low per-call budget so
each individual measurement is bounded:

```yaml
- name: Benchmark sanity
  timeout-minutes: 10
  run: |
    lake build bench
    lake exe bench list
    lake exe bench run goodFib \
      --max-seconds-per-call 0.5 --param-ceiling 1024
```

`run`, `compare`, and `verify` enforce their own per-call wallclock
cap via the in-Lean kill path, so they don't need an external
`timeout(1)` wrapper — and the example above runs unchanged on
Windows, which lacks GNU `timeout`. `list` and `lake build` aren't
covered by the per-call cap (they don't run user code), so for an
overall job-level bound use the CI system's own timeout (e.g.
GitHub Actions' `timeout-minutes` shown above) rather than a shell
wrapper. The same per-call flags work on `compare`, so you can pin a
comparison run to a tight wallclock budget without recompiling. See
[quickstart.md](quickstart.md#configuring-a-benchmark) for the full
flag list and the matching declaration-time `where { … }` syntax.

When a benchmark's natural ladder is far larger than CI allows, ship
a tighter default in the source itself rather than putting flags in
every CI job:

```lean
setup_benchmark expensive n => 2 ^ n where {
  maxSecondsPerCall := 0.25
  paramCeiling := 256
}
```

CLI flags still layer on top, so the ergonomic split is "declaration
defaults what's true forever; CLI flags what's true today."
