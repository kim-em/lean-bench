import Lean
import LeanBench.Core

/-!
# `LeanBench.Stats` — ratios + verdict

Per the v0.1 spec: the verdict is intentionally weak. The raw
`ratios` array is the source of truth; the verdict is decoration.

This module is pure: it operates on a `BenchmarkSpec`, the collected
`DataPoint`s, and the user's complexity closure, all of which are
passed in by `Run.lean`. No registries are consulted here.
-/

open Lean

namespace LeanBench
namespace Stats

/-- Compute the per-point ratio C = perCallNanos / complexity(param).
    Skips param=0 and param=1 (denominator may be 0 or 1, noisy),
    any non-`ok` data points, any rows flagged as not part of the
    verdict (doubling-probe rows in `.linear` mode), and any rows
    whose total wall time was dominated by spawn overhead
    (`belowSignalFloor` — see issue #15). -/
def ratiosFromPoints
    (complexity : Nat → Nat)
    (points : Array DataPoint) :
    Array Ratio := Id.run do
  let mut acc : Array Ratio := #[]
  for dp in points do
    if dp.param < 2 then continue
    if dp.status != .ok then continue
    if !dp.partOfVerdict then continue
    if dp.belowSignalFloor then continue
    let d := complexity dp.param
    if d == 0 then continue
    acc := acc.push (dp.param, dp.perCallNanos / d.toFloat)
  return acc

/-- OLS slope of the points `(x, y)`. `none` if there are fewer than
    two points, the `x` values are (essentially) all equal, or the
    `x` range is too narrow to make the slope estimate trustworthy
    (a linear-ladder log-x span of `~0.65` is not enough to tell
    `n^k` from `n^(k+0.1)` with realistic noise). The caller is
    expected to fall back to a range check on `cMin`/`cMax`. -/
private def fitSlope (pts : Array (Float × Float)) : Option Float :=
  if pts.size < 2 then none
  else Id.run do
    let n := pts.size.toFloat
    let mut sx : Float := 0.0
    let mut sy : Float := 0.0
    let mut xMin : Float := pts[0]!.1
    let mut xMax : Float := pts[0]!.1
    for (x, y) in pts do
      sx := sx + x
      sy := sy + y
      if x < xMin then xMin := x
      if x > xMax then xMax := x
    if xMax - xMin < 1.0 then return none
    let xbar := sx / n
    let ybar := sy / n
    let mut num : Float := 0.0
    let mut den : Float := 0.0
    for (x, y) in pts do
      let dx := x - xbar
      num := num + dx * (y - ybar)
      den := den + dx * dx
    if den < 1e-12 then return none
    return some (num / den)

/-- Reduce the ratios into the v0.1 weak verdict.

    `warmupFraction ∈ [0, 1)` is the fraction of leading ratios to drop
    before fitting, so the cold regime (where per-spawn overhead
    dominates the declared complexity) doesn't corrupt the slope.
    Clamped to leave at least 3 samples.

    The verdict is decided by the log-log slope β of `C` vs `param`
    over the trimmed tail: `|β| ≤ slopeTolerance` → consistent, else
    inconclusive. On narrow log-x ranges (where the slope fit is
    ill-conditioned and rejected by `fitSlope`), falls back to a
    multiplicative range check: `cMax / cMin ≤ max(noiseFloor,
    exp(slopeTolerance · xRange))`. The `noiseFloor` admits the
    realistic spread of single-shot near-cap measurements (15-20%)
    that exponential-complexity benchmarks unavoidably hit. `cMin`/
    `cMax` are still reported as diagnostic context. Returns
    `(verdict, cMin?, cMax?, slope?, droppedLeading)`. -/
def deriveVerdict (ratios : Array Ratio)
    (warmupFraction slopeTolerance noiseFloor : Float) :
    Verdict × Option Float × Option Float × Option Float × Nat :=
  if ratios.size < 3 then
    (.inconclusive, none, none, none, 0)
  else
    let raw := (ratios.size.toFloat * warmupFraction).toUInt64.toNat
    let maxDrop := ratios.size - 3
    let dropCount := min raw maxDrop
    let kept := ratios.extract dropCount ratios.size
    let cs := kept.map (·.2)
    let cMin := cs.foldl min cs[0]!
    let cMax := cs.foldl max cs[0]!
    let pts : Array (Float × Float) := kept.filterMap fun (p, c) =>
      if c > 0.0 && p ≥ 1 then
        some (Float.log p.toFloat, Float.log c)
      else
        none
    let slope? := fitSlope pts
    let v : Verdict := match slope? with
      | some β =>
        if β.abs ≤ slopeTolerance then .consistentWithDeclaredComplexity
        else .inconclusive
      | none =>
        -- Slope was rejected (narrow log-x range or degenerate).
        -- Fall back to a multiplicative range check matching the
        -- slope semantics: log(cMax/cMin) ≤ slopeTolerance · xRange.
        -- Equivalent to `cMax/cMin ≤ exp(slopeTolerance · xRange)`.
        -- Naive `cMax/cMin ≤ 1 + slopeTolerance` is unsound — it
        -- admits e.g. an actual `n² log n` declared as `n²` because
        -- `log` varies slowly over any narrow `n` interval.
        if pts.size < 2 || cMin ≤ 0.0 then .inconclusive
        else
          let xs := pts.map (·.1)
          let xMin := xs.foldl min xs[0]!
          let xMax := xs.foldl max xs[0]!
          let xRange := xMax - xMin
          let scaled := Float.exp (slopeTolerance * xRange)
          let bound := if scaled > noiseFloor then scaled else noiseFloor
          if cMax / cMin ≤ bound then
            .consistentWithDeclaredComplexity
          else .inconclusive
    (v, some cMin, some cMax, slope?, dropCount)

/-- Compute the `Advisory` array for a result, given the post-spawn-floor
    annotated points. Pure — exposed so `runBenchmark` can call it once
    `belowSignalFloor` flags are settled, and so unit tests can drive
    it without spawning subprocesses.

    Order matters: a single result can carry multiple advisories
    (e.g. `belowSignalFloor` + `truncatedAtCap`); the format renders
    them in the order produced here, which matches the order users
    typically want to see them — global quality issues first
    (everything below floor / everything capped), then partial /
    structural issues (some below floor, ladder truncated, too few
    verdict rows). -/
def computeAdvisories (points : Array DataPoint) (verdictRows : Nat) :
    Array Advisory := Id.run do
  let mut out : Array Advisory := #[]
  -- Count `ok` rows (including any flagged below-floor) and how many
  -- of those `ok` rows were below the signal floor. A row that errored
  -- isn't a candidate for "below signal floor" advice — its data is
  -- missing, not noise.
  let okPoints := points.filter (·.status == .ok)
  let belowFloor := okPoints.filter (·.belowSignalFloor)
  if okPoints.isEmpty then
    -- No `ok` rows at all. If every row hit the cap, that's
    -- `allCapped`; if every row errored, the result already
    -- communicates that without an advisory (the table shows
    -- `[error: ...]` per row).
    let cappedRows := points.filter fun dp =>
      dp.status == .timedOut || dp.status == .killedAtCap
    if !cappedRows.isEmpty && cappedRows.size == points.size then
      out := out.push .allCapped
  else if belowFloor.size == okPoints.size then
    out := out.push .belowSignalFloor
  else if belowFloor.size > 0 then
    out := out.push (.partiallyBelowSignalFloor belowFloor.size okPoints.size)
  -- Truncated-at-cap: at least one `ok` row landed AND somewhere in
  -- the ladder a row hit the cap. The truncation point is the param
  -- of the first capped row.
  if !okPoints.isEmpty then
    let firstCapped := points.findSome? fun dp =>
      match dp.status with
      | .timedOut | .killedAtCap => some dp.param
      | _ => none
    match firstCapped with
    | some p => out := out.push (.truncatedAtCap p)
    | none => pure ()
  -- Verdict resolution: too few rows survive the warmup trim + floor
  -- filter for the slope fit / range check to be meaningful.
  if verdictRows < 3 && !okPoints.isEmpty then
    out := out.push (.tooFewVerdictRows verdictRows)
  return out

/-- Build a `BenchmarkResult` from spec + complexity closure + collected points.

    `points` should already carry `belowSignalFloor` annotations;
    `runBenchmark` sets those after measuring the spawn floor and
    before calling `summarize`. The verdict ratios and the advisory
    list are both derived from the same annotated points, so the
    "fewer than 3 verdict rows" advisory exactly matches what
    `deriveVerdict` saw. -/
def summarize
    (spec : BenchmarkSpec)
    (complexity : Nat → Nat)
    (points : Array DataPoint) :
    BenchmarkResult :=
  let ratios := ratiosFromPoints complexity points
  let (verdict, cMin?, cMax?, slope?, dropped) :=
    deriveVerdict ratios spec.config.verdictWarmupFraction
      spec.config.slopeTolerance spec.config.narrowRangeNoiseFloor
  let kept := ratios.size - dropped
  let advisories := computeAdvisories points kept
  { function := spec.name
  , complexityFormula := spec.complexityFormula
  , hashable := spec.hashable
  , config := spec.config
  , points
  , ratios
  , verdict
  , cMin?
  , cMax?
  , verdictDroppedLeading := dropped
  , slope?
  , advisories }

end Stats
end LeanBench
