import LeanBench

/-!
# `AutoFit` unit tests (issue #8)

Pure tests against the `LeanBench.AutoFit` ranking. We synthesize
`(param, perCallNanos)` samples from a known catalog model and assert
the corresponding catalog entry wins the rank with a `decisive`
confidence verdict. This sidesteps subprocess spawning so the suite
is fast.

Coverage:

- `O(n)` data → best fit `n`, decisive.
- `O(n^2)` data → best fit `n^2`, decisive.
- `O(n^3)` data → best fit `n^3`, decisive.
- `O(2^n)` data → best fit `2^n`, decisive.
- `O(n log n)` data → best fit `n * Nat.log2 (n + 1)`, decisive.
- `O(1)` data → best fit `1`, decisive.
- Empty / single-point input → empty rank with `Confidence.none`.
- Two-rung input → ranks but `weak` verdict (rung floor).
- Narrow log-x range → `weak` verdict (runner-up close).
- The catalog labels match `setup_benchmark` syntax (so a user can
  paste the suggestion straight into a declaration).
- `2^n` log evaluator handles huge `n` without building `Nat`s
  (regression: an earlier draft built `2^n` as a `Nat` then converted
  to `Float` and dropped on overflow).
-/

open LeanBench
open LeanBench.AutoFit

namespace LeanBench.Test.AutoFit

/-- Compute `M(n)` from a catalog entry's `logFn`, for synthesizing
    test inputs in `Float`. We can't recover the exact `Nat` value
    (the catalog only stores `log M`), but `exp(logFn n)` is the
    same number scaled to `Float`, which is exactly what we want for
    timing data. Returns `0.0` if the entry has no value at `n`. -/
private def evalModel (label : String) (n : Nat) : Float :=
  match catalog.find? (fun m => m.label == label) with
  | Option.none => 0.0
  | some m =>
    match m.logFn n with
    | Option.none => 0.0
    | some lm => Float.exp lm

/-- Build samples `(p, c · M(p))` for the catalog `M` named by `label`.
    `c` is the multiplicative constant. No noise — synthetic data so
    the test is deterministic. -/
private def synth (label : String) (c : Float) (params : Array Nat) :
    Array (Nat × Float) :=
  params.filterMap fun p =>
    let mp := evalModel label p
    if mp ≤ 0.0 then Option.none
    else some (p, c * mp)

/-- Standard polynomial doubling ladder, capped where `Nat.toFloat`
    starts losing precision but still wide enough to discriminate
    catalog entries. -/
private def polyParams : Array Nat :=
  #[2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384]

/-- Linear-sweep ladder for exponential models: small linear range
    where `2^n` doesn't overflow `Float`. -/
private def expParams : Array Nat :=
  #[5, 10, 15, 20, 25, 30, 35, 40]

private def assertDecisive (label : String) (samples : Array (Nat × Float)) :
    IO UInt32 := do
  let ranking := rank samples
  if ranking.fits.isEmpty then
    IO.eprintln s!"FAIL [{label}]: empty rank"
    return 1
  let best := ranking.fits[0]!
  match ranking.confidence with
  | .decisive => pure ()
  | .weak reason =>
    IO.eprintln s!"FAIL [{label}]: expected decisive verdict, got weak ({reason})"
    return 1
  | .none =>
    IO.eprintln s!"FAIL [{label}]: confidence none on non-empty fits"
    return 1
  if best.model.label == label then return 0
  IO.eprintln s!"FAIL [{label}]: expected best fit {label}, got {best.model.label} (stdLogC={best.stdLogC})"
  for f in ranking.fits do
    IO.eprintln s!"  {f.model.label}: stdLogC={f.stdLogC} meanC={f.meanC} n={f.okPoints}"
  return 1

def testLinear : IO UInt32 := assertDecisive "n" (synth "n" 5.0 polyParams)
def testQuadratic : IO UInt32 := assertDecisive "n^2" (synth "n^2" 0.05 polyParams)
def testCubic : IO UInt32 := assertDecisive "n^3" (synth "n^3" 0.001 polyParams)
def testExponential : IO UInt32 := assertDecisive "2^n" (synth "2^n" 1.0 expParams)
def testNLogN : IO UInt32 :=
  assertDecisive "n * Nat.log2 (n + 1)"
    (synth "n * Nat.log2 (n + 1)" 7.0 polyParams)
def testConstant : IO UInt32 := assertDecisive "1" (synth "1" 12.5 polyParams)

/-- An empty input must return an empty rank with `.none` confidence. -/
def testEmpty : IO UInt32 := do
  let r := rank #[]
  if r.fits.isEmpty then
    match r.confidence with
    | .none => return 0
    | _ =>
      IO.eprintln s!"FAIL: empty fits but non-none confidence"
      return 1
  IO.eprintln s!"FAIL: expected empty rank for empty samples; got {r.fits.size} entries"
  return 1

/-- A single sample isn't enough to compute a stddev; rank must be empty. -/
def testSinglePoint : IO UInt32 := do
  let r := rank #[(8, 100.0)]
  if r.fits.isEmpty then return 0
  IO.eprintln s!"FAIL: expected empty rank for one sample; got {r.fits.size} entries"
  return 1

/-- Two usable rungs is the absolute minimum for a stddev, but the
    confidence verdict must mark such a result `weak` (rung-count
    floor of 3) so the formatter does NOT crown a winner. -/
def testTwoRungsWeak : IO UInt32 := do
  let r := rank (synth "n" 5.0 #[2, 4])
  if r.fits.isEmpty then
    IO.eprintln "FAIL: expected non-empty fits for 2 rungs"
    return 1
  match r.confidence with
  | .weak _ => return 0
  | .decisive =>
    IO.eprintln s!"FAIL: 2-rung input should not be decisive; winner {r.fits[0]!.model.label} okPoints={r.fits[0]!.okPoints}"
    return 1
  | .none =>
    IO.eprintln "FAIL: confidence none on non-empty fits"
    return 1

/-- The `2^n` catalog entry must NOT silently win on linear data even
    when the ladder runs into the range where `2^n` overflows
    `Float`. With log-domain evaluation there's no overflow at all,
    but the test guards the regression: an earlier draft used a
    `Nat → Nat` catalog and produced a degenerate `stdLogC = 0` for
    `2^n` because `log ∞` collapsed every residual. -/
def testExponentialOverflowDropped : IO UInt32 := do
  -- A wide ladder going to 65536 with linear timing data. The
  -- log-domain `2^n` entry must produce a finite, large stdLogC
  -- (linear data fit by exponential model = bad fit) and lose to `n`.
  let samples := synth "n" 5.0 #[2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536]
  let r := rank samples
  if r.fits.isEmpty then
    IO.eprintln "FAIL: expected non-empty rank"
    return 1
  let best := r.fits[0]!
  if best.model.label == "2^n" then
    IO.eprintln s!"FAIL: 2^n incorrectly won on linear data (overflow guard regression). stdLogC={best.stdLogC}"
    return 1
  -- `n` must win and use the full ladder (16 rungs).
  if best.model.label != "n" then
    IO.eprintln s!"FAIL: expected n to win; got {best.model.label}"
    return 1
  if best.okPoints != 16 then
    IO.eprintln s!"FAIL: expected n to fit 16 rungs (the full common set); got {best.okPoints}"
    return 1
  return 0

/-- Confidence regression: a hand-built fit cluster where the
    runner-up is within the cushion threshold (1.5×) of the winner
    must report `weak`, not `decisive`. We bypass `rank` to keep the
    test independent of catalog noise — `assessConfidence` is the
    unit under test here. -/
def testRunnerUpCloseWeak : IO UInt32 := do
  let m := catalog[0]!
  let fits : Array Fit :=
    -- Winner stdLogC = 0.5, runner-up = 0.6. Cushion = max(0.10, 0.5*0.5) = 0.25.
    -- 0.6 < 0.5 + 0.25 = 0.75, so the verdict must be weak.
    #[ { model := m, okPoints := 8, meanC := 1.0, stdLogC := 0.5 }
     , { model := m, okPoints := 8, meanC := 2.0, stdLogC := 0.6 } ]
  match assessConfidence fits with
  | .weak _ => return 0
  | _ =>
    IO.eprintln "FAIL: close runner-up should produce weak verdict"
    return 1

/-- Confidence regression: when the runner-up clears the 1.5×
    cushion, the verdict is `decisive`. Mirror of
    `testRunnerUpCloseWeak`. -/
def testRunnerUpFarDecisive : IO UInt32 := do
  let m := catalog[0]!
  let fits : Array Fit :=
    -- Winner stdLogC = 0.5, runner-up = 1.0. Cushion = 0.25.
    -- 1.0 ≥ 0.5 + 0.25 = 0.75, so verdict is decisive.
    #[ { model := m, okPoints := 8, meanC := 1.0, stdLogC := 0.5 }
     , { model := m, okPoints := 8, meanC := 2.0, stdLogC := 1.0 } ]
  match assessConfidence fits with
  | .decisive => return 0
  | v =>
    IO.eprintln s!"FAIL: clear runner-up gap should produce decisive verdict; got {repr v}"
    return 1

/-- Codex review concern: with the prior `Nat → Nat` design, models
    with cheaper-to-evaluate `M` discarded fewer high-end rungs, and
    `stdLogC` was scored over different rung sets. With the
    log-domain catalog, every model evaluates on the *same* common
    rung set — every `Fit.okPoints` must be equal. -/
def testCommonRungSet : IO UInt32 := do
  let samples := synth "n" 5.0 polyParams
  let r := rank samples
  if r.fits.size < 2 then
    IO.eprintln "FAIL: expected ≥ 2 fits"
    return 1
  let counts := r.fits.map (·.okPoints)
  let head := counts[0]!
  if counts.all (· == head) then return 0
  IO.eprintln s!"FAIL: okPoints differs across catalog entries: {counts.toList}"
  return 1

/-- Catalog labels must round-trip through `setup_benchmark` syntax —
    i.e. the label is a literal complexity expression a user could
    paste after `=>`. We can't elaborate at runtime, but we can at
    least pin the exact string set so a refactor doesn't silently
    change what users see. -/
def testCatalogLabels : IO UInt32 := do
  let expected : Array String :=
    #["1", "n", "n * Nat.log2 (n + 1)", "n^2", "n^3", "2^n"]
  let actual := catalog.map (·.label)
  if actual == expected then return 0
  IO.eprintln s!"FAIL: catalog labels changed; expected {expected.toList}, got {actual.toList}"
  return 1

/-- Smoke test for `samplesFromResult`: a hand-rolled
    `BenchmarkResult` with populated `trialSummaries` produces samples
    keyed by `medianPerCallNanos`. -/
def testSamplesFromResult : IO UInt32 := do
  let summaries : Array TrialSummary :=
    #[ { param := 8,  okCount := 1, medianPerCallNanos := 100.0
       , minPerCallNanos := 100.0, maxPerCallNanos := 100.0
       , relativeSpread := 0.0 }
     , { param := 16, okCount := 1, medianPerCallNanos := 200.0
       , minPerCallNanos := 200.0, maxPerCallNanos := 200.0
       , relativeSpread := 0.0 } ]
  let result : BenchmarkResult :=
    { function := `dummy
    , complexityFormula := "n"
    , hashable := false
    , config := {}
    , points := #[]
    , ratios := #[]
    , verdict := .inconclusive
    , cMin? := Option.none
    , cMax? := Option.none
    , trialSummaries := summaries }
  let samples := samplesFromResult result
  if samples == #[(8, 100.0), (16, 200.0)] then return 0
  IO.eprintln s!"FAIL: samplesFromResult mismatched; got {samples}"
  return 1

/-- Regression test: huge param values (where the prior Nat-domain
    catalog would have built astronomical Nats and stalled the
    ranker) must complete in fractions of a second under the
    log-domain catalog. The test simply asserts the call returns. -/
def testHugeParamsFast : IO UInt32 := do
  -- 2^131072 as a Nat would be ~40000 decimal digits and computing it
  -- alongside several smaller models would dominate the run. With the
  -- log-domain catalog this is constant work.
  let samples := synth "n" 5.0 #[2, 4, 8, 16, 32, 1048576, 16777216, 268435456]
  let _ := rank samples
  return 0

def runTests : IO UInt32 := do
  let mut anyFail : UInt32 := 0
  for (label, t) in
    [ ("catalog labels", testCatalogLabels),
      ("empty rank",     testEmpty),
      ("single point",   testSinglePoint),
      ("two rungs weak", testTwoRungsWeak),
      ("runner-up close weak", testRunnerUpCloseWeak),
      ("runner-up far decisive", testRunnerUpFarDecisive),
      ("samples from result", testSamplesFromResult),
      ("common rung set", testCommonRungSet),
      ("constant best",  testConstant),
      ("linear best",    testLinear),
      ("nlogn best",     testNLogN),
      ("quadratic best", testQuadratic),
      ("cubic best",     testCubic),
      ("exponential best", testExponential),
      ("exponential overflow dropped", testExponentialOverflowDropped),
      ("huge params fast", testHugeParamsFast) ]
  do
    let code ← t
    if code != 0 then
      IO.eprintln s!"FAIL: {label}"
      anyFail := 1
    else
      IO.println s!"  ok  {label}"
  if anyFail == 0 then
    IO.println "auto-fit tests passed"
  return anyFail

end LeanBench.Test.AutoFit

def main : IO UInt32 := LeanBench.Test.AutoFit.runTests
