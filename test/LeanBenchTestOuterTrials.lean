import LeanBench

/-!
# Outer trials + per-param trial summaries (issue #4)

Pure unit tests for the issue #4 plumbing: `BenchmarkConfig.outerTrials`,
`Stats.medianFloat`, `Stats.ratiosFromPoints` aggregating multiple trials
per param via the median, `Stats.trialSummariesFromPoints` shape, and
`Format.fmtResult` rendering of the per-param summary block.

Plus one end-to-end run via the same trivially-fast XOR benchmark used
by `LeanBenchTestE2E`, with `outerTrials := 3`, asserting that:
- the result has exactly `outerTrials` per-param data points for ok
  rungs
- `trialSummaries` has one entry per ok param with the right `okCount`
- the formatter shows the trial-summary block when `outerTrials > 1`
-/

open LeanBench

namespace LeanBench.Test.OuterTrials

/-- A trivially-fast `Nat → UInt64` (same shape as the e2e benchmark). -/
def littleFnTrials (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 1
  for _ in [0:n] do
    x := x ^^^ n.toUInt64
  return x

setup_benchmark littleFnTrials n => n where {
  maxSecondsPerCall := 0.5, paramCeiling := 8, targetInnerNanos := 50_000_000
  signalFloorMultiplier := 1.0
  outerTrials := 3
}

end LeanBench.Test.OuterTrials

private def trialsName : Lean.Name := `LeanBench.Test.OuterTrials.littleFnTrials

private def expectEq {α} [BEq α] [Repr α] (label : String) (got want : α) :
    IO Unit := do
  unless got == want do
    throw <| .userError
      s!"{label}: expected {repr want}, got {repr got}"

private def containsSub (haystack needle : String) : Bool :=
  ((haystack.splitOn needle).length) > 1

private def mkOk (param : Nat) (perCall : Float) (trial : Nat := 0) : DataPoint :=
  { param, innerRepeats := 1, totalNanos := perCall.toUInt64.toNat
  , perCallNanos := perCall, resultHash := some 1
  , status := .ok, trialIndex := trial }

/-! ## `BenchmarkConfig.validate` rejects `outerTrials = 0`. -/

example : (BenchmarkConfig.validate { outerTrials := 0 }).isOk = false := by native_decide
example : (BenchmarkConfig.validate { outerTrials := 5 }).isOk = true := by native_decide

/-! ## `ConfigOverride.outerTrials?` round-trips through `apply`. -/

example :
    (({ outerTrials? := some 7 } : ConfigOverride).apply
      ({ outerTrials := 1 } : BenchmarkConfig)).outerTrials = 7 := rfl

example :
    (({} : ConfigOverride).apply
      ({ outerTrials := 4 } : BenchmarkConfig)).outerTrials = 4 := rfl

/-! ## `Stats.medianFloat` -/

def testMedianFloatOdd : IO UInt32 := do
  let xs : Array Float := #[3.0, 1.0, 2.0]
  let m := Stats.medianFloat xs
  unless m == 2.0 do
    IO.eprintln s!"median odd: expected 2.0, got {m}"
    return 1
  return 0

def testMedianFloatEvenUpperMiddle : IO UInt32 := do
  -- Upper middle on even: index 2 of [1,2,3,4] is 3.0.
  let xs : Array Float := #[4.0, 1.0, 3.0, 2.0]
  let m := Stats.medianFloat xs
  unless m == 3.0 do
    IO.eprintln s!"median even: expected 3.0 (upper middle), got {m}"
    return 1
  return 0

def testMedianFloatEmpty : IO UInt32 := do
  let xs : Array Float := #[]
  let m := Stats.medianFloat xs
  unless m == 0.0 do
    IO.eprintln s!"median empty: expected 0.0, got {m}"
    return 1
  return 0

/-! ## `ratiosFromPoints` collapses trials per param via median. -/

def testRatiosUseMedianAcrossTrials : IO UInt32 := do
  -- Three trials at param=4, two trials at param=8. The middle (median)
  -- of each cluster should drive the per-param ratio.
  let pts : Array DataPoint := #[
    mkOk 4 100.0 (trial := 0),
    mkOk 4 200.0 (trial := 1),
    mkOk 4 300.0 (trial := 2),
    mkOk 8 350.0 (trial := 0),
    mkOk 8 450.0 (trial := 1) ]
  -- complexity = identity (n).
  let comp : Nat → Nat := fun n => n
  let rs := Stats.ratiosFromPoints comp pts
  expectEq "ratios.size" rs.size 2
  -- median(100,200,300) = 200; ratio at param=4 is 200/4 = 50.
  expectEq "ratios.0.param" rs[0]!.1 4
  unless rs[0]!.2 == 50.0 do
    IO.eprintln s!"expected ratio at 4 to be 50.0, got {rs[0]!.2}"
    return 1
  -- Even-length cluster at 8: upper middle of (350, 450) = 450; ratio
  -- 450/8 = 56.25.
  expectEq "ratios.1.param" rs[1]!.1 8
  unless rs[1]!.2 == 56.25 do
    IO.eprintln s!"expected ratio at 8 to be 56.25 (upper-middle median), got {rs[1]!.2}"
    return 1
  return 0

def testRatiosSkipNonOkTrials : IO UInt32 := do
  -- A failed trial at param=4 must not count toward the median: median
  -- of just (100, 300) — the two ok trials — is 300 (upper middle).
  let pts : Array DataPoint := #[
    mkOk 4 100.0 (trial := 0),
    { param := 4, innerRepeats := 0, totalNanos := 0, perCallNanos := 0.0
    , resultHash := none, status := .killedAtCap, trialIndex := 1 },
    mkOk 4 300.0 (trial := 2) ]
  let rs := Stats.ratiosFromPoints (fun n => n) pts
  expectEq "ratios.size after skip" rs.size 1
  -- 300 / 4 = 75.0
  unless rs[0]!.2 == 75.0 do
    IO.eprintln s!"expected ratio 75.0 (median of ok trials only), got {rs[0]!.2}"
    return 1
  return 0

/-! ## `trialSummariesFromPoints` -/

def testTrialSummariesShape : IO UInt32 := do
  let pts : Array DataPoint := #[
    mkOk 4 100.0 (trial := 0),
    mkOk 4 200.0 (trial := 1),
    mkOk 4 300.0 (trial := 2),
    mkOk 8 500.0 (trial := 0) ]
  let ss := Stats.trialSummariesFromPoints pts
  expectEq "summaries.size" ss.size 2
  expectEq "s0.param" ss[0]!.param 4
  expectEq "s0.okCount" ss[0]!.okCount 3
  unless ss[0]!.medianPerCallNanos == 200.0 do
    IO.eprintln s!"s0.median: expected 200.0, got {ss[0]!.medianPerCallNanos}"
    return 1
  unless ss[0]!.minPerCallNanos == 100.0 do
    IO.eprintln s!"s0.min: expected 100.0, got {ss[0]!.minPerCallNanos}"
    return 1
  unless ss[0]!.maxPerCallNanos == 300.0 do
    IO.eprintln s!"s0.max: expected 300.0, got {ss[0]!.maxPerCallNanos}"
    return 1
  -- (max - min) / median = (300 - 100) / 200 = 1.0.
  unless ss[0]!.relativeSpread == 1.0 do
    IO.eprintln s!"s0.spread: expected 1.0, got {ss[0]!.relativeSpread}"
    return 1
  -- A single-trial cluster has spread = 0, min = max = median.
  expectEq "s1.okCount" ss[1]!.okCount 1
  unless ss[1]!.relativeSpread == 0.0 do
    IO.eprintln s!"s1.spread: expected 0.0 on single trial, got {ss[1]!.relativeSpread}"
    return 1
  return 0

/-! ## `Format.fmtResult` shows the trial-summary block iff outerTrials > 1. -/

def testFmtResultHidesSummaryWhenOuterTrialsOne : IO UInt32 := do
  let pts : Array DataPoint := #[mkOk 4 200.0]
  let result : BenchmarkResult :=
    { function := `Sample.fast
    , complexityFormula := "n"
    , hashable := true
    , config := { outerTrials := 1 }
    , points := pts
    , ratios := #[(4, 50.0)]
    , verdict := .inconclusive
    , cMin? := some 50.0
    , cMax? := some 50.0
    , verdictDroppedLeading := 0
    , slope? := none
    , spawnFloorNanos? := some 100
    , advisories := #[]
    , trialSummaries := Stats.trialSummariesFromPoints pts }
  let rendered := Format.fmtResult result
  if containsSub rendered "trial summaries" then
    IO.eprintln s!"expected NO trial-summary block when outerTrials=1, got:\n{rendered}"
    return 1
  -- Per-trial annotation must also be absent in the single-trial case.
  if containsSub rendered "[trial 1/1]" then
    IO.eprintln s!"expected NO [trial k/N] markers when outerTrials=1, got:\n{rendered}"
    return 1
  return 0

def testFmtResultShowsSummaryWhenOuterTrialsGtOne : IO UInt32 := do
  let pts : Array DataPoint := #[
    mkOk 4 100.0 (trial := 0),
    mkOk 4 200.0 (trial := 1),
    mkOk 4 300.0 (trial := 2) ]
  let summaries := Stats.trialSummariesFromPoints pts
  let result : BenchmarkResult :=
    { function := `Sample.trio
    , complexityFormula := "n"
    , hashable := true
    , config := { outerTrials := 3 }
    , points := pts
    , ratios := #[(4, 50.0)]
    , verdict := .inconclusive
    , cMin? := some 50.0
    , cMax? := some 50.0
    , verdictDroppedLeading := 0
    , slope? := none
    , spawnFloorNanos? := some 100
    , advisories := #[]
    , trialSummaries := summaries }
  let rendered := Format.fmtResult result
  for needle in
    ["trial summaries (3 trials per param", "spread", "[trial 1/3]",
     "[trial 2/3]", "[trial 3/3]"] do
    unless containsSub rendered needle do
      IO.eprintln s!"missing '{needle}' in:\n{rendered}"
      return 1
  return 0

/-! ## End-to-end: a real outerTrials=3 run produces the right shape. -/

def testEndToEndOuterTrialsThree : IO UInt32 := do
  let result ← runBenchmark trialsName
  -- The benchmark is registered with outerTrials=3; every successful
  -- rung should have its three measurements in `points`.
  if result.points.isEmpty then
    IO.eprintln "expected at least one measured point"
    return 1
  -- Group ok points by param, assert each group has 3 entries — every
  -- rung that completed should run the full trial cluster (the harness
  -- bails out only on the first failed trial).
  let okPoints := result.points.filter (·.status == .ok)
  if okPoints.isEmpty then
    IO.eprintln s!"expected at least one ok point; got: {repr (result.points.map (·.status))}"
    return 1
  let okParams : Array Nat := okPoints.map (·.param)
  let distinct : Array Nat := okParams.foldl (init := #[]) fun acc p =>
    if acc.contains p then acc else acc.push p
  for p in distinct do
    let countAtP := okPoints.filter (·.param == p) |>.size
    unless countAtP == 3 do
      IO.eprintln s!"param={p} had {countAtP} ok trials; expected 3 (outerTrials=3)"
      return 1
  -- Trial indices must be 0,1,2 within each rung's ok cluster.
  for p in distinct do
    let indices : Array Nat := okPoints.filterMap fun dp =>
      if dp.param == p then some dp.trialIndex else none
    let sorted := indices.qsort (· < ·)
    unless sorted == #[0, 1, 2] do
      IO.eprintln s!"param={p} trial indices: expected #[0,1,2], got {sorted}"
      return 1
  -- Trial summaries: one entry per ok param.
  unless result.trialSummaries.size == distinct.size do
    IO.eprintln s!"expected {distinct.size} trial summaries, got {result.trialSummaries.size}"
    return 1
  for s in result.trialSummaries do
    unless s.okCount == 3 do
      IO.eprintln s!"trial-summary at param={s.param} okCount={s.okCount}, expected 3"
      return 1
    unless s.minPerCallNanos ≤ s.medianPerCallNanos
        ∧ s.medianPerCallNanos ≤ s.maxPerCallNanos do
      IO.eprintln s!"trial-summary at param={s.param}: min/median/max ordering broken"
      return 1
  return 0

/-! ## CLI flag round-trip -/

def testCliOuterTrialsFlag : IO UInt32 := do
  match LeanBench.Cli.topCmd.process
      ["run", "littleFnTrials", "--outer-trials", "5"] with
  | .ok (_, parsed) =>
    let ovr := LeanBench.Cli.configOverrideFromParsed parsed
    expectEq "cli.outerTrials" ovr.outerTrials? (some 5)
    return 0
  | .error (_, msg) =>
    IO.eprintln s!"unexpected parse failure: {msg}"
    return 1

/-! ## Driver -/

def runTests : IO UInt32 := do
  let mut anyFail : UInt32 := 0
  for (label, t) in
    [ ("median.odd", testMedianFloatOdd),
      ("median.evenUpperMiddle", testMedianFloatEvenUpperMiddle),
      ("median.empty", testMedianFloatEmpty),
      ("ratios.medianAcrossTrials", testRatiosUseMedianAcrossTrials),
      ("ratios.skipNonOk", testRatiosSkipNonOkTrials),
      ("trialSummaries.shape", testTrialSummariesShape),
      ("fmtResult.hidesSummaryWhenOne", testFmtResultHidesSummaryWhenOuterTrialsOne),
      ("fmtResult.showsSummaryWhenMore", testFmtResultShowsSummaryWhenOuterTrialsGtOne),
      ("cli.outerTrialsFlag", testCliOuterTrialsFlag),
      ("e2e.outerTrialsThree", testEndToEndOuterTrialsThree) ]
  do
    let code ←
      try t
      catch e => do
        IO.eprintln s!"FAIL: {label}: {e.toString}"
        pure 1
    if code != 0 then
      IO.eprintln s!"FAIL: {label}"
      anyFail := 1
    else
      IO.println s!"  ok  {label}"
  if anyFail == 0 then
    IO.println "outer-trials tests passed"
  return anyFail

/-- Same compiled binary is parent (driver) and child (single batch);
    `_child` first arg distinguishes them. -/
def main (args : List String) : IO UInt32 :=
  match args with
  | "_child" :: _ => LeanBench.Cli.dispatch args
  | _ => runTests
