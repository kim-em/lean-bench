# Advanced

Beyond the basics in [quickstart.md](quickstart.md). All of this is
v0.1; v0.2+ items live in [PLAN.md](../PLAN.md).

## Comparing implementations

`lake exe bench compare A B C [...]` runs each named benchmark
through its own doubling ladder, then prints:

- the full per-function table (one block per benchmark)
- the `commonParams` intersection (the params at which every
  function produced an `ok` row)
- `agreement` over those params: `allAgreed`, `divergedAt …`, or
  `hashUnavailable` (when at least one function's return type lacks
  `Hashable`)

A function whose ladder stops early (because its complexity rises
faster) doesn't break the comparison — its rows are preserved and
the agreement check just skips the params it didn't reach.

The Hashable opt-in: `setup_benchmark` synthesizes `Hashable α`
once at registration. If it succeeds, every measurement records the
hash and `compare` checks them. If not, `runChecked` still forces
the result (so the benchmark measures real work) but writes
`result_hash := none`, and `compare` reports `hashUnavailable`.

## Reading the verdict

The raw `ratios` array `[(param, perCallNanos / complexity(param)), …]`
is the source of truth. The verdict is a heuristic decoration:

- `consistentWithDeclaredComplexity` if `cMax / cMin ≤ 4` over ≥3
  data points past `param = 2`
- `inconclusive` otherwise

`inconclusive` is not "the implementation is wrong" — it just means
the model + measurement noise doesn't support the strong claim.
Things that produce `inconclusive`:

- declared complexity is wrong (e.g. `n` for an O(n²) algorithm)
- declared complexity is too coarse (e.g. `2^n` for an algorithm
  whose actual cost is `1.6^n` — these differ by a polynomial
  factor that shows up over a doubling ladder)
- cache effects (smooth at small n, jumps at L1 / L2 boundaries)
- arbitrary-precision arithmetic on growing results (Lean's `Nat`
  is bignum; if `f n` returns a number with k bits, every operation
  on it costs O(k))

When the verdict is `inconclusive`, look at the raw ratios. They
tell you which way C is drifting (growing → algorithm is slower
than declared; shrinking → declared model is too pessimistic).

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

A typical CI step runs the bench exe with low budget per call so the
whole job stays bounded:

```yaml
- name: Benchmark sanity
  run: |
    lake build bench
    timeout 60 lake exe bench list                # always passes
    timeout 120 lake exe bench run goodFib --max-seconds-per-call 0.5
```

The `--max-seconds-per-call` plumbing is on v0.2 (CLI flags for
config overrides). v0.1 you set it via `setup_benchmark … where { … }`
when v0.2 lands; or just rebuild with smaller `BenchmarkConfig`
defaults locally.
