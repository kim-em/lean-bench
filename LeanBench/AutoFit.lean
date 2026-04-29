import LeanBench.Core

/-!
# `LeanBench.AutoFit` — heuristic complexity model selection (issue #8)

Given a set of `(param, perCallNanos)` measurements, rank a fixed
catalog of candidate complexity models by goodness-of-fit. The user
sees a "best fit" suggestion plus runner-ups; the v0.1 declared-model
workflow is unchanged.

**This is a heuristic, not a proof.** A model fits well when
`C = perCallNanos / model(param)` is approximately constant across
the ladder. Concretely, we score each candidate by the standard
deviation of `log C` — i.e. how much the fitted constant has to vary
to explain the observations. The smallest dispersion wins.

## Catalog

| label                     | function                  |
|---------------------------|---------------------------|
| `1`                       | `fun _ => 1`              |
| `n`                       | `fun n => n`              |
| `n * Nat.log2 (n + 1)`    | `fun n => n * Nat.log2 (n + 1)` |
| `n^2`                     | `fun n => n^2`            |
| `n^3`                     | `fun n => n^3`            |
| `2^n`                     | `fun n => 2^n`            |

The catalog is intentionally small. Symbolic complexity inference is
explicitly a non-goal (per the issue); a documented short list is
enough for the "I don't know what this is, suggest something" use
case.

## Method

Each catalog entry is described by a `logFn : Nat → Option Float`
that computes `log M(n)` directly — `n * log 2` for `2^n`, `2 * log n`
for `n^2`, etc. — *never* materialising the underlying `Nat`. This
matters for two reasons:

- **Comparable scores.** All models evaluate on the same rung set
  (no rung is skipped because `M(p)` happens to overflow `Float`),
  so `stdLogC` is apples-to-apples across catalog entries.
- **Cheap.** Building `2^65536` as a `Nat` and then converting to
  `Float` is needlessly expensive; the log form is constant time.

For each candidate model `M`:

- Build the per-rung residual `r_i = log t_i - logFn(p_i)` over
  rungs satisfying `p_i ≥ 2 ∧ t_i > 0 ∧ logFn(p_i) = some _`.
- Report `meanC = exp(mean r_i)` (a representative `C`) and
  `stdLogC = stddev(r_i)` (the dispersion). Lower dispersion =
  tighter fit.

`p_i ≥ 2` mirrors `Stats.ratiosFromPoints` so the same warmup-cold
rungs that don't enter the verdict don't bias the auto-fit either.

## Confidence

A small log-x range can't tell `n` from `n log n`. The `Verdict` (in
`Confidence`) reflects this: we crown a winner only when there are
at least 3 usable rungs and the runner-up's `stdLogC` is meaningfully
worse than the winner's. Otherwise the report tags the result as
inconclusive — the user sees the same ranked list but no `←` arrow.
-/

namespace LeanBench
namespace AutoFit

/-- One candidate model in the auto-fit catalog. -/
structure Model where
  /-- Display label, matching what a user would write in
      `setup_benchmark NAME n => <label>`. -/
  label : String
  /-- `log M(n)` evaluated directly in `Float`. Returns `none` when
      `M(n)` is undefined (e.g. `log 0`). All catalog entries are
      defined for `n ≥ 2`, but the option type lets us short-circuit
      degenerate inputs cleanly. The point of returning `log M` (not
      `M`) is to avoid building enormous `Nat`s for `2^n` /  `n^k`
      with huge `n` and to keep the fit numerically stable on long
      ladders. -/
  logFn : Nat → Option Float
  deriving Inhabited

/-- Result of fitting a single catalog entry to the observations. -/
structure Fit where
  model    : Model
  /-- Number of `(param, perCallNanos)` pairs that contributed. -/
  okPoints : Nat
  /-- Geometric mean of `perCallNanos / model(param)` across the
      contributing samples. The "constant" the model wants. -/
  meanC    : Float
  /-- Standard deviation of `log C` across the contributing samples.
      Lower = tighter fit. The score we rank by. -/
  stdLogC  : Float
  deriving Inhabited

/-- Confidence verdict for the ranking. The catalog is fixed-size and
    nested-monotone (every entry is asymptotically distinct), so the
    quality of the pick depends entirely on how much the ladder
    discriminates them. We don't try to give a probability — just an
    actionable signal:

    - `decisive`: enough rungs survived AND the runner-up is
      meaningfully worse. The formatter prints `←` on the winner.
    - `weak`: at least one model fit, but the evidence isn't strong
      enough to crown a winner — too few rungs, or the top two scores
      are close enough that the ladder probably can't separate them.
      The formatter prints the table without an arrow and adds a
      hint line.
    - `none`: nothing fit at all (fewer than 2 usable rungs for
      every catalog entry). -/
inductive Confidence
  | decisive
  | weak (reason : String)
  | none
  deriving Repr, Inhabited

/-- The ranked output of `rank` plus the confidence verdict. -/
structure Ranking where
  fits       : Array Fit
  confidence : Confidence
  deriving Inhabited

/-- The fixed catalog (issue #8). Order is informational; ranking is
    by fit score, not list position. The labels are the strings the
    user would write after `=>` in `setup_benchmark` so that copying
    the suggested model into a declaration is mechanical.

    Each `logFn` returns `log M(n)` directly. We special-case `n < 2`
    to `none` for log-domain entries to avoid `log 0 = -∞`; `1` and
    `2^n` are defined everywhere but we apply the same minimum so
    the rungs that vote are the same across catalog entries (and
    match `Stats.ratiosFromPoints`'s `param ≥ 2` filter). -/
def catalog : Array Model :=
  let log2 : Float := Float.log 2.0
  #[
  -- M(n) = 1, log M = 0.
  { label := "1", logFn := fun n => if n < 2 then Option.none else some 0.0 },
  -- M(n) = n, log M = log n.
  { label := "n", logFn := fun n =>
      if n < 2 then Option.none else some (Float.log n.toFloat) },
  -- M(n) = n * log2(n+1). For n ≥ 2, log2(n+1) ≥ 1, so M ≥ 2 > 0.
  { label := "n * Nat.log2 (n + 1)", logFn := fun n =>
      if n < 2 then Option.none else
      let l2 := (Nat.log2 (n + 1)).toFloat
      if l2 ≤ 0.0 then Option.none else
      some (Float.log n.toFloat + Float.log l2) },
  -- M(n) = n^2, log M = 2·log n.
  { label := "n^2", logFn := fun n =>
      if n < 2 then Option.none else some (2.0 * Float.log n.toFloat) },
  -- M(n) = n^3, log M = 3·log n.
  { label := "n^3", logFn := fun n =>
      if n < 2 then Option.none else some (3.0 * Float.log n.toFloat) },
  -- M(n) = 2^n, log M = n·log 2. Never builds the Nat 2^n.
  { label := "2^n", logFn := fun n =>
      if n < 2 then Option.none else some (n.toFloat * log2) }
  ]

/-- The shared rung set: every (param, perCallNanos) sample for which
    *every* catalog entry yields a finite `log M`. We score all
    candidates on the same set so their `stdLogC` values are directly
    comparable.

    With the log-domain catalog above, every entry is defined and
    finite for `n ≥ 2`, so this is exactly `samples` filtered to
    `p ≥ 2 ∧ t > 0 ∧ t.isFinite`. The intersection logic is kept
    explicit anyway: it tolerates a future catalog entry whose
    `logFn` is `none` on some rungs without silently shifting the
    rung set per model. -/
def commonSamples (samples : Array (Nat × Float)) : Array (Nat × Float) :=
  samples.filter fun (p, t) =>
    p ≥ 2 && t > 0.0 && t.isFinite &&
    catalog.all (fun m => (m.logFn p).isSome)

/-- Fit one catalog entry to the *common* sample set. Returns `none`
    if fewer than 2 usable rungs survive. By construction (see
    `commonSamples`), every model's `logFn` is `some` on every rung
    here, so `okPoints == common.size` for every successful fit. -/
def fitOne (model : Model) (common : Array (Nat × Float)) : Option Fit :=
  Id.run do
    let mut residuals : Array Float := #[]
    for (p, t) in common do
      match model.logFn p with
      | Option.none => continue
      | some lm =>
        let r := Float.log t - lm
        if !r.isFinite then continue
        residuals := residuals.push r
    if residuals.size < 2 then return Option.none
    let nF := residuals.size.toFloat
    let mean := residuals.foldl (· + ·) 0.0 / nF
    let var := residuals.foldl (init := 0.0) fun acc r =>
      let d := r - mean
      acc + d * d
    let std := (var / nF).sqrt
    return some {
      model
      okPoints := residuals.size
      meanC := Float.exp mean
      stdLogC := std
    }

/-- Decide whether the ranking is decisive enough to crown a winner.

    Heuristic, not a probability: the catalog is small and densely
    populated near the typical "between O(n) and O(n²)" workload, so
    the user is best served by a clear "we don't have enough data"
    signal when the top two are close.

    Rules:

    - Need ≥ 3 usable rungs (the verdict's own resolution floor —
      below that, even a perfectly clean fit is statistically
      indistinguishable from noise).
    - The runner-up's `stdLogC` must exceed the winner's by a
      multiplicative cushion (winner × `gapFactor`), with an
      absolute floor (`gapFloor`) so a near-zero winner score
      doesn't trivially clear a zero gap. The defaults
      (`gapFactor = 0.5`, `gapFloor = 0.10`) crown a winner when
      the runner-up's score is ≥ 1.5× the winner's, which separates
      the catalog cleanly on doubling ladders of length ≥ 8 (see
      the `Fib` example smoke tests) without flagging genuinely
      indistinguishable ladders as decisive. -/
def assessConfidence (fits : Array Fit) : Confidence :=
  if fits.isEmpty then .none
  else
    let winner := fits[0]!
    if winner.okPoints < 3 then
      .weak s!"only {winner.okPoints} usable rung(s) — need ≥ 3 for a confident pick"
    else if fits.size < 2 then
      .decisive
    else
      let runnerUp := fits[1]!
      let gapFactor : Float := 0.5
      let gapFloor : Float := 0.10
      let cushion := max gapFloor (gapFactor * winner.stdLogC)
      if runnerUp.stdLogC ≥ winner.stdLogC + cushion then
        .decisive
      else
        .weak s!"runner-up '{runnerUp.model.label}' is within noise of '{winner.model.label}'; the ladder cannot separate them"

/-- Rank the catalog by fit quality (lowest log-residual stddev first)
    and return a confidence verdict.

    All models are scored on `commonSamples` so their `stdLogC` values
    are directly comparable. The first entry of `fits` is the best
    candidate; subsequent entries are runner-ups in increasing order
    of dispersion. -/
def rank (samples : Array (Nat × Float)) : Ranking :=
  let common := commonSamples samples
  let fits := catalog.filterMap (fitOne · common)
  let sorted := fits.qsort (fun a b => a.stdLogC < b.stdLogC)
  { fits := sorted, confidence := assessConfidence sorted }

/-- Build the sample set from a `BenchmarkResult`'s trial summaries
    (one median per param, matching the same rows the verdict's
    `ratios` array sees). Falls back to per-row points when the
    summaries are empty (a result built outside `runBenchmark`,
    e.g. tests). -/
def samplesFromResult (r : BenchmarkResult) : Array (Nat × Float) :=
  if !r.trialSummaries.isEmpty then
    r.trialSummaries.map (fun s => (s.param, s.medianPerCallNanos))
  else
    -- Tests / hand-rolled results may bypass `Stats.summarize` and
    -- not populate `trialSummaries`. Fall back to filtering the raw
    -- points the way `Stats.ratiosFromPoints` does.
    r.points.filterMap fun dp =>
      if dp.status == .ok && dp.partOfVerdict && !dp.belowSignalFloor then
        some (dp.param, dp.perCallNanos)
      else Option.none

end AutoFit
end LeanBench
