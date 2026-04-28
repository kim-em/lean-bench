# Lean-specific benchmarking pitfalls

A microbenchmark in Lean 4 measures wall time on compiled code. That
sounds simple, but Lean's runtime — bignum `Nat`, reference-counted
allocation, structural sharing, ahead-of-time compilation versus
elaborator evaluation — has enough sharp edges that a benchmark can
silently measure something other than the algorithm you wrote down.
This guide walks through the failure modes we have hit ourselves,
each with a concrete example you can run.

`lean-bench` papers over some of these (it forces every result, hashes
when it can, and runs in a clean child process for each measurement)
but it cannot save you from a benchmark whose declared parameter
doesn't actually drive the work, or whose return value's bignum
arithmetic dominates the algorithm. Knowing what the harness does and
does not handle is the point of this document.

For background on the harness mechanics referenced below, see
[design.md](design.md). For the user-facing setup, see
[quickstart.md](quickstart.md).

## 1. `Nat` is bignum: arithmetic cost depends on the value

Lean's `Nat` is arbitrary-precision. Every `+`, `*`, `/` runs in
roughly `O(k)` (or `O(k log k)` for multiplication) where `k` is the
bit width of the operands. A loop that "obviously" performs `n`
additions can easily turn into `O(n²)` total work if the
accumulator's bit width grows linearly per iteration.

Compare:

```lean
-- Repeated doubling: after iteration i the accumulator is 2^i, so
-- it has Θ(i) bits. Per-iteration `s + s` costs Θ(i). Total work:
-- Θ(n^2), not Θ(n).
def doubleN (n : Nat) : Nat := Id.run do
  let mut s : Nat := 1
  for _ in [0:n] do s := s + s
  return s

-- Same shape, fixed-width arithmetic. Per-iteration cost is constant
-- (truncates mod 2^64), and the loop is genuinely Θ(n).
def doubleU64 (n : Nat) : UInt64 := Id.run do
  let mut s : UInt64 := 1
  for _ in [0:n] do s := s + s
  return s

setup_benchmark doubleN   n => n  -- declares O(n); β comes back ≈ 1
setup_benchmark doubleU64 n => n  -- declares O(n); β comes back ≈ 0
```

`doubleN`'s verdict line will report a positive log-log slope β
near 1, because the actual growth is `n²` while you declared `n`.
The fix is either to tighten the model
(`setup_benchmark doubleN n => n * n`) or to switch to fixed-width
arithmetic (`UInt64`, `USize`) when the benchmark's intent is to
measure something other than bignum cost.

The same trap surfaces less obviously in:

- **factorials, products on large numbers** — `n!` has `Θ(n log n)`
  bits, so even a "single" `*` near the end of the computation
  costs more than the entire start of the loop.
- **gcds on growing inputs** — Euclidean steps on `k`-bit numbers
  cost `O(k²)` per step in the schoolbook implementation.
- **`n.factorial`-shaped helpers used as test inputs** — the input
  size in bits, not the value of `n`, is what the algorithm sees.

A subtler case: a loop that sums `0 + 1 + … + (n-1)` looks like it
should hit the same trap, but the partial sum is `Θ(log n)`-bit
wide for most iterations (the value reaches `n²/2`, but its bit
width is only `Θ(log n)`). Total work is `Θ(n log n)`, not `Θ(n²)`.
The bignum penalty bites when the *bit width* of the accumulator
grows linearly per step, not just its value.

When in doubt, declare the complexity that includes the bignum cost
(`n * n` for an `O(n)` loop whose accumulator grows linearly), or
switch to `UInt64` / `USize` / `Float` if the algorithm is supposed
to be doing fixed-width work and the bignum drift is incidental.

## 2. Force the work: don't accidentally time a thunk

Lean is call-by-value at runtime, but the compiler is aggressive. A
benchmark that constructs a value and never observes it can be
optimised down to nothing — or, worse, partially elided so the
"measurement" tracks loop overhead rather than the algorithm. Two
ways this happens in practice:

**Loop-invariant hoisting.** If your inner loop calls a pure function
on a loop-invariant argument, the compiler can lift the call out:

```lean
-- Naive timing loop. Compiler sees `f param` as loop-invariant and
-- hoists it out: you've timed one call plus N empty iterations.
for _ in [0:N] do
  let _ := f param
```

`lean-bench`'s generated loop body routes every result through
`LeanBench.blackBox`, a `@[noinline]` consumer with a real side
effect ([Env.lean:46](../LeanBench/Env.lean)). The compiler can't
prove the call is dead, so the argument can't be elided. If you're
writing your own timing harness without `lean-bench`, you need an
equivalent: a global ref, an `IO` print of the result, or anything
the compiler must assume is observable.

**Lazy data structures.** Lean's `thunk`s and `Lazy` values aren't
forced by binding them to a name. A benchmark that returns
`Thunk.mk fun _ => expensive` and times "the call" measures
allocation of a closure, not `expensive`. The harness defends against
this by hashing the result when the return type has `Hashable`
(which forces deep evaluation of any thunk it traverses), or by
calling `sizeOf` on the result when it doesn't. Both paths walk
enough of the value to defeat thunk-based laziness for ordinary
data; neither is bullet-proof against bespoke laziness (a custom
type that wraps an `IO` action it never runs, say).

The take-away: for `lean-bench`, pick a return type that is
`Hashable` whenever you can — `compare` will also be able to use the
hash for cross-implementation agreement checks. If you must return
something non-`Hashable`, ensure the function's body actually
constructs the value rather than capturing it in a closure.

## 3. Hashing and result observation are not free

The harness's "did we observe the result" mechanism is `Hashable.hash
result` (or `Hashable.hash (sizeOf result)` as a fallback). For
small or fixed-width return types this is a few ns and you can
ignore it. Lean core's `Hashable` instances vary in cost:

- **`Hashable Nat`** is `UInt64.ofNat`, so a single bignum hashes in
  constant time regardless of bit width. Containers are still
  linear in *length*: a `List Nat` of length `k` costs `Θ(k)` to
  hash, not `Θ(k * limbs)`.
- **`Hashable String`** walks the whole string. Long strings are a
  real cost.
- **`Hashable ByteArray`** likewise walks every byte.
- **derived `Hashable` on a recursive type** walks the whole tree.
  A balanced tree of depth `d` with `n = 2^d` leaves takes `Θ(n)`
  hash time.

When the result-observation cost is comparable to or larger than the
algorithm cost, the verdict reflects the hash, not the algorithm.
Two practical responses:

- **Measure intermediate work explicitly.** Have the benchmark
  return a small summary (the array's `sum`, the polynomial's
  degree, a `UInt64` rolling hash) instead of the full structure.
  The summary needs to actually depend on every part of the work,
  otherwise the compiler may DCE the parts you don't observe.
- **Use `with prep := …` for setup that should not be re-timed.**
  See [quickstart.md](quickstart.md#optional-per-param-setup-with-with-prep--).
  The prep result is hashed once, before timing begins, so its
  construction cost is paid up front and not folded into every
  inner iteration.

`lean-bench`'s `compare` reports `agreement` over the common-param
intersection: hashes either match (`allAgreed`) or diverge at named
triples. A function whose return type is not `Hashable` falls back
to "hashUnavailable" — the comparison still runs, but you lose the
cross-implementation correctness check. Prefer `Hashable`-bearing
return types when you have a choice.

## 4. Allocation, sharing, and reference counting

Lean uses reference counting with destructive update for unique
references. This matters two different ways for benchmarks.

**Allocation cost in `Nat` arithmetic and structural data.** Every
non-trivial `Nat` operation allocates (a fresh bignum result), as
does every `Array.push`, `List.cons`, `String` concatenation, and
record construction with at least one field changed. A loop that
looks like `arr := arr.push x` is `O(1)` amortised when `arr` is
unshared (Lean re-uses the buffer in place) and `O(n)` per push
when it is shared (Lean copies first). Code that benchmarks fine in
isolation can flip between regimes when used in a context that
holds a second reference to the same array.

A concrete example:

```lean
def pushN (n : Nat) : Array Nat := Id.run do
  let mut a : Array Nat := #[]
  for i in [0:n] do a := a.push i
  return a

-- Looks like O(n). It is, when called as below.
setup_benchmark pushN n => n

-- But this version, which keeps a snapshot every step, is O(n^2):
def pushNShared (n : Nat) : Array Nat := Id.run do
  let mut a : Array Nat := #[]
  let mut snapshots : Array (Array Nat) := #[]
  for i in [0:n] do
    snapshots := snapshots.push a   -- shares `a`!
    a := a.push i                   -- next push must copy
  return a
```

If you suspect sharing-induced regressions, write a smaller
allocation-only variant that exercises just the suspect operation
and benchmark it alongside the full algorithm — the relative
per-call cost will tell you whether the unique-reference path is
being hit.

**Allocation churn vs. arithmetic in tight loops.** Even when a
single operation is cheap, allocating a fresh bignum / array per
iteration generates GC-style pressure that doesn't map cleanly onto
a textbook complexity. The harness measures wall time including
this pressure, so it shows up as "constant factor C is bigger than
you expected" rather than as a slope deviation. If you're chasing a
2× constant-factor difference between two algorithms with the same
declared complexity, allocation patterns are a strong suspect.

The `alloc_bytes` and `peak_rss_kb` schema fields are reserved for a
future feature ([#6](https://github.com/kim-em/lean-bench/issues/6))
that will surface this directly. Until then, allocation effects are
visible only through the `C = perCallNanos / complexity(n)` constant.

## 5. Compiled code is not the only way Lean evaluates

`lean-bench` measures compiled-code wall time, full stop
([advanced.md](advanced.md#what-this-library-is-not)). It does not
measure:

- **`#eval`** — runs in the elaborator, not native code, and can
  be much slower than the compiled equivalent on numeric work.
- **`decide` / `Decidable` reduction at type-check time** — uses the
  kernel reducer, which is slower still and has very different
  cost-per-step characteristics from compiled code.
- **`reduce` / `simp`** — symbolic, not computational. The cost
  model is "size of the term, number of rewrites" rather than
  "wall-clock time of a compiled function".
- **kernel typechecking** — has its own performance regime, dominated
  by definitional unfolding rather than by the algorithm.

Two consequences. First, you cannot use `#eval` (or a tactic that
calls into the elaborator) as a sanity check that your benchmark is
fast enough. Compile it, run it, look at `lean-bench list / verify`.
Second, performance regressions visible in `simp` or `decide` aren't
the same regressions a compiled-code benchmark catches — these are
distinct phenomena and need distinct tooling. If you're chasing a
compile-time slowdown, you want kernel- or elaborator-level tracing
rather than a `lean-bench` row.

The corollary: a function being benchmarked must compile. `noncomputable`
defs, defs that depend on `Classical.choice`, and proofs-as-programs
(`PSigma.fst`-style extraction from non-decidable existentials) won't
be measurable. The failure surfaces when building or running the
compiled benchmark, not during `setup_benchmark` elaboration.

## 6. Warm versus cold: read the trimmed tail, not the first row

Every benchmark run has a cold regime where the per-call overhead
(child startup, JIT-style adaptive code paths in the runtime, cache
fills, branch predictor warmup) dominates the algorithm. The first
few rungs of the ladder are systematically slower than the
asymptotic regime would predict. The harness reports per-call wall
time including this overhead and trims the leading
`verdictWarmupFraction` (20% by default) of ratios before fitting
the slope ([quickstart.md](quickstart.md#configuring-a-benchmark)).

What this means in practice:

- **Don't read the first few rows as algorithm data.** Tombstones
  (`†`) on early rungs in the report mark the trim region. The
  trimmed tail is the source of truth for the verdict; the
  early rows are kept for context only.
- **The per-spawn floor sets a hard noise floor.** Every report
  prints `per-spawn floor: X ms`. Any data point with `total_nanos`
  smaller than ~10× the floor is noise rather than signal. The
  auto-tuner usually drives `inner_repeats` up enough that batches
  hit `targetInnerNanos := 500ms`, which is well above the floor;
  but for very fast operations on small `param` you can see flat or
  non-monotone per-call times until `param` grows past where startup
  dominates.
- **A single child process per param means no warm cache *between*
  params.** Each measurement is a fresh subprocess, by design (so
  the wall cap is enforceable without external `timeout(1)`). The
  flip side is no L1/L2 carry-over, no JIT-style steady state across
  rungs. The default `warm` mode does still amortise within a rung —
  the auto-tuner runs the function many times inside a single
  spawn, so caches and branch predictors reach steady state for that
  param. If you want the cold per-call cost (cache refill on every
  measurement, no internal averaging), use `--cache-mode cold` and
  read [advanced.md#cache-modes](advanced.md#cache-modes). The two
  modes measure different things; either can be appropriate.

For exponential-complexity benchmarks, the ladder shifts from
doubling to a linear sweep over `(lastOk, firstFail)` and the
log-x range narrows. The slope fit is rejected for narrow ranges
and the verdict falls back to a multiplicative range check —
`cMax / cMin ≤ max(narrowRangeNoiseFloor, exp(slopeTolerance ·
xRange))`, see [advanced.md](advanced.md#reading-the-verdict).
On those benchmarks the verdict's `β` line shows `—` and you
should read `cMin/cMax` directly.

## 7. Fixed-problem benchmarks: median, not mean

For a `setup_fixed_benchmark` registration there is no parameter
sweep. The harness runs one warmup call followed by `repeats`
measured calls (default 5) and reports median, min, max, plus a
hash-agreement check across repeats
([quickstart.md](quickstart.md#fixed-problem-benchmarks)). Two
things to watch for:

- **Non-determinism shows up as hash divergence.** Different hashes
  across repeats of the same benchmark mean the function isn't
  pure (or its `Hashable` instance is unstable). The report flags
  this as `hashes diverged`. Fix the benchmark before trusting any
  of its timing numbers.
- **The first measured call is post-warmup but still cold-ish.** A
  single warmup is enough to load FFI libraries and trigger module
  init, but for an algorithm with its own internal caching (memo
  tables, lazy initialisation) the first measured call may still
  be slower than the rest. Reading the median rather than the mean
  defends against this naturally; raise `repeats` if min/max
  spread on your hardware is too wide for a 5-sample median.

## Summary

The mistakes are concrete, not abstract. If your verdict is
inconclusive or your `C` constant looks suspicious:

1. Is the return type a bignum, or built from bignums? You may be
   measuring `Nat` arithmetic rather than your algorithm.
2. Is the result actually observed? `lean-bench` forces every
   result — via `Hashable.hash` when the return type has a
   `Hashable` instance, via `Hashable.hash (sizeOf …)` otherwise.
   `result_hash` in the JSONL row is `null` only when the return
   type is non-`Hashable` (which disables cross-implementation
   agreement checking, but does not disable forcing).
3. Is the result observation itself dominating the timing? Switch
   to a smaller summary return type if so.
4. Are sharing effects flipping the algorithm between cheap and
   expensive paths? Test in isolation.
5. Are you reading early-rung rows? Look at the trimmed tail.
6. Is the benchmark fixed-problem? Read median, not mean, and
   confirm hashes agree across repeats.

When in doubt, the raw `ratios` array in the JSONL output is the
source of truth. The verdict is decoration.
