import LeanBench

/-!
# Edge-case advisories + custom ladder (issue #15)

Pure unit tests for the issue #15 plumbing: `Advisory`, the
`belowSignalFloor` per-row flag, the `MeasurementQuality` summary on
`BenchmarkResult`, and the `.custom` parameter ladder.

Everything in this file runs without spawning a subprocess — it
operates on hand-built `DataPoint` arrays and drives `Stats.summarize`
/ `annotateBelowSignalFloor` / `Format.fmtResult` directly. The
end-to-end "the harness produces these advisories on a real run" test
lives in the existing `LeanBenchTestE2E` file (which has its own
trivial-fast benchmark and disables the floor filter).
-/

open LeanBench

private def mkOk (param : Nat) (totalNanos : Nat) (perCall : Float) : DataPoint :=
  { param, innerRepeats := 1, totalNanos, perCallNanos := perCall
  , resultHash := some 1, status := .ok }

private def mkCapped (param : Nat) : DataPoint :=
  { param, innerRepeats := 0, totalNanos := 0, perCallNanos := 0.0
  , resultHash := none, status := .killedAtCap }

private def expectEq {α} [BEq α] [Repr α] (label : String) (got want : α) :
    IO Unit := do
  unless got == want do
    throw <| .userError
      s!"{label}: expected {repr want}, got {repr got}"

private def containsSub (haystack needle : String) : Bool :=
  ((haystack.splitOn needle).length) > 1

/-! ## `annotateBelowSignalFloor` -/

/-- Rows whose `totalNanos < multiplier × spawnFloor` get flagged;
    others are left alone. Errored rows are never flagged because
    their `totalNanos` is meaningless. -/
def testAnnotateBelowSignalFloor : IO UInt32 := do
  -- Spawn floor 10ms, multiplier 10×, threshold 100ms.
  let floor : Option Nat := some 10_000_000
  let mul : Float := 10.0
  let pts : Array DataPoint := #[
    mkOk 2  50_000_000  (50_000_000.0),  -- below threshold
    mkOk 4 200_000_000 (200_000_000.0),  -- above threshold
    mkCapped 8                            -- non-ok, never flagged
  ]
  let annotated : Array DataPoint :=
    annotateBelowSignalFloor mul floor pts
  expectEq "below.row0.flag"  annotated[0]!.belowSignalFloor true
  expectEq "below.row1.flag"  annotated[1]!.belowSignalFloor false
  expectEq "below.row2.flag"  annotated[2]!.belowSignalFloor false
  -- multiplier ≤ 1 disables the check entirely.
  let disabled : Array DataPoint :=
    annotateBelowSignalFloor 1.0 floor pts
  for dp in disabled do
    expectEq "disabled.flag" dp.belowSignalFloor false
  -- Missing spawn-floor reading also leaves everything unflagged.
  let noFloor : Array DataPoint :=
    annotateBelowSignalFloor mul none pts
  for dp in noFloor do
    expectEq "noFloor.flag" dp.belowSignalFloor false
  return 0

/-! ## `Stats.computeAdvisories` -/

/-- All ok rows below the floor → `belowSignalFloor` advisory. -/
def testAdvisoryAllBelowFloor : IO UInt32 := do
  let pts : Array DataPoint := #[
    { mkOk 2 1 1.0 with belowSignalFloor := true },
    { mkOk 4 2 2.0 with belowSignalFloor := true } ]
  let advs := Stats.computeAdvisories pts 0
  -- Expect at least the `belowSignalFloor` advisory; tooFewVerdictRows
  -- may also fire because the verdict has zero rows after filtering,
  -- but `belowSignalFloor` is the headline classification.
  unless advs.contains .belowSignalFloor do
    IO.eprintln s!"expected belowSignalFloor advisory, got {repr advs}"
    return 1
  return 0

/-- Mixed below-floor + ok-above-floor → `partiallyBelowSignalFloor`. -/
def testAdvisoryPartialBelowFloor : IO UInt32 := do
  let pts : Array DataPoint := #[
    { mkOk 2 1 1.0 with belowSignalFloor := true },
    mkOk 4 200_000_000 200.0,
    mkOk 8 400_000_000 400.0 ]
  let advs := Stats.computeAdvisories pts 2
  unless advs.contains (.partiallyBelowSignalFloor 1 3) do
    IO.eprintln s!"expected partiallyBelowSignalFloor 1 3, got {repr advs}"
    return 1
  return 0

/-- All rows hit the cap → `allCapped`. -/
def testAdvisoryAllCapped : IO UInt32 := do
  let pts : Array DataPoint := #[mkCapped 2, mkCapped 4, mkCapped 8]
  let advs := Stats.computeAdvisories pts 0
  unless advs.contains .allCapped do
    IO.eprintln s!"expected allCapped, got {repr advs}"
    return 1
  -- truncatedAtCap requires at least one ok row — must NOT fire here.
  for adv in advs do
    match adv with
    | .truncatedAtCap _ =>
      IO.eprintln s!"truncatedAtCap should not fire when allCapped: {repr advs}"
      return 1
    | _ => pure ()
  return 0

/-- Some rows ok, then capped → `truncatedAtCap` with the firstFail param. -/
def testAdvisoryTruncatedAtCap : IO UInt32 := do
  let pts : Array DataPoint := #[
    mkOk 2 200_000_000 100.0,
    mkOk 4 400_000_000 100.0,
    mkCapped 8 ]
  let advs := Stats.computeAdvisories pts 2
  unless advs.contains (.truncatedAtCap 8) do
    IO.eprintln s!"expected truncatedAtCap 8, got {repr advs}"
    return 1
  return 0

/-- Few rows survive the verdict → `tooFewVerdictRows`. -/
def testAdvisoryTooFewVerdictRows : IO UInt32 := do
  let pts : Array DataPoint := #[
    mkOk 2 200_000_000 100.0,
    mkOk 4 400_000_000 100.0 ]
  let advs := Stats.computeAdvisories pts 2
  unless advs.contains (.tooFewVerdictRows 2) do
    IO.eprintln s!"expected tooFewVerdictRows 2, got {repr advs}"
    return 1
  return 0

/-- A clean run with enough rows above the floor → no advisories. -/
def testAdvisoryOrdinaryRunIsEmpty : IO UInt32 := do
  let pts : Array DataPoint := #[
    mkOk 2 200_000_000 100.0,
    mkOk 4 400_000_000 100.0,
    mkOk 8 800_000_000 100.0,
    mkOk 16 1_600_000_000 100.0 ]
  let advs := Stats.computeAdvisories pts 4
  unless advs.isEmpty do
    IO.eprintln s!"expected empty advisories on ordinary run, got {repr advs}"
    return 1
  return 0

/-! ## `Format.fmtResult` rendering of advisories + per-row floor mark -/

/-- A row with `belowSignalFloor := true` carries a `[<floor]`
    annotation in the rendered table. -/
def testFmtResultBelowFloorMark : IO UInt32 := do
  let pts : Array DataPoint := #[
    { mkOk 2 1 1.0 with belowSignalFloor := true },
    mkOk 4 200_000_000 100.0 ]
  let result : BenchmarkResult :=
    { function := `Sample.fast
    , complexityFormula := "n"
    , hashable := true
    , config := { signalFloorMultiplier := 10.0 }
    , points := pts
    , ratios := #[(4, 25.0)]
    , verdict := .inconclusive
    , cMin? := some 25.0
    , cMax? := some 25.0
    , verdictDroppedLeading := 0
    , slope? := none
    , spawnFloorNanos? := some 5_000_000
    , advisories := #[.partiallyBelowSignalFloor 1 2] }
  let rendered := Format.fmtResult result
  unless containsSub rendered "[<floor]" do
    IO.eprintln s!"expected `[<floor]` mark in:\n{rendered}"
    return 1
  unless containsSub rendered "1/2 ok rows were below" do
    IO.eprintln s!"expected partial advisory in:\n{rendered}"
    return 1
  return 0

/-- The advisory line is rendered with the `‼` lead and a concrete
    actionable suggestion. -/
def testFmtResultAdvisoryLines : IO UInt32 := do
  let dummyResult : BenchmarkResult :=
    { function := `Sample.fast
    , complexityFormula := "n"
    , hashable := true
    , config := { maxSecondsPerCall := 1.0, signalFloorMultiplier := 10.0 }
    , points := #[mkOk 2 1 1.0]
    , ratios := #[]
    , verdict := .inconclusive
    , cMin? := none
    , cMax? := none
    , verdictDroppedLeading := 0
    , slope? := none
    , spawnFloorNanos? := some 100
    , advisories := #[
        .belowSignalFloor,
        .allCapped,
        .truncatedAtCap 16,
        .tooFewVerdictRows 1 ] }
  let rendered := Format.fmtResult dummyResult
  let needles : List String := [
    "‼ every measurement was below",
    "‼ every measurement hit the wallclock cap",
    "‼ ladder stopped at param=16 after hitting the wallclock cap",
    "‼ only 1 verdict-eligible row",
    -- Suggestions actually point at the new knobs.
    "paramSchedule := .custom",
    "--max-seconds-per-call",
    "--param-ceiling",
    "setup_fixed_benchmark" ]
  for needle in needles do
    unless containsSub rendered needle do
      IO.eprintln s!"missing '{needle}' in:\n{rendered}"
      return 1
  return 0

/-! ## `.custom` ladder semantics

Pure config layer — drive `runBenchmark` with the `.custom` schedule
and assert the points correspond to the user-specified params. The
benchmark is the same trivial XOR loop the e2e test uses, with the
floor filter disabled so the ladder shape is what's under test. -/

namespace LeanBench.Test.Advisory

def littleFnCustom (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 1
  for _ in [0:n] do
    x := x ^^^ n.toUInt64
  return x

setup_benchmark littleFnCustom n => n where {
  maxSecondsPerCall := 0.5
  targetInnerNanos := 50_000_000
  signalFloorMultiplier := 1.0
  paramSchedule := .custom #[3, 5, 7, 11]
}

end LeanBench.Test.Advisory

private def customName : Lean.Name := `LeanBench.Test.Advisory.littleFnCustom

/-- A `.custom` ladder runs exactly the user-supplied params, in
    order, with no doubling probe and no auto-bracketing. -/
def testCustomLadder : IO UInt32 := do
  let result ← runBenchmark customName
  let observed := result.points.map (·.param)
  -- All-ok prefix — the params are tiny, nothing should hit the cap.
  -- The ladder is exactly the declared list.
  expectEq "custom.params" observed #[3, 5, 7, 11]
  return 0

/-- An empty `.custom` ladder is rejected up front by config validation
    — better than producing an empty result silently. -/
def testCustomEmptyRejected : IO UInt32 := do
  let badCfg : BenchmarkConfig := { paramSchedule := .custom #[] }
  match badCfg.validate with
  | .error msg =>
    if containsSub msg "empty" then return 0
    IO.eprintln s!"expected 'empty' in error, got: {msg}"
    return 1
  | .ok _ =>
    IO.eprintln "expected validate to reject .custom #[]"
    return 1

/-- Duplicate params in `.custom` are rejected by validation: the
    formatter keys `C` lookup by `param`, so two rows with the same
    param would render with the same ratio and the same warmup
    dagger, giving a misleading report. Codex flagged this in the
    issue #15 review. -/
def testCustomDuplicateRejected : IO UInt32 := do
  let badCfg : BenchmarkConfig := { paramSchedule := .custom #[1, 2, 2, 4] }
  match badCfg.validate with
  | .error msg =>
    if containsSub msg "duplicate" then return 0
    IO.eprintln s!"expected 'duplicate' in error, got: {msg}"
    return 1
  | .ok _ =>
    IO.eprintln "expected validate to reject .custom with duplicate params"
    return 1

/-- `signalFloorMultiplier < 1.0` is rejected by validation. -/
def testSignalFloorMultiplierValidated : IO UInt32 := do
  let badCfg : BenchmarkConfig := { signalFloorMultiplier := 0.5 }
  match badCfg.validate with
  | .error msg =>
    if containsSub msg "signalFloorMultiplier" then return 0
    IO.eprintln s!"expected signalFloorMultiplier message, got: {msg}"
    return 1
  | .ok _ =>
    IO.eprintln "expected validate to reject signalFloorMultiplier < 1.0"
    return 1

/-! ## Driver -/

def runTests : IO UInt32 := do
  let mut anyFail : UInt32 := 0
  for (label, t) in
    [ ("annotateBelowSignalFloor", testAnnotateBelowSignalFloor),
      ("advisory.allBelowFloor",   testAdvisoryAllBelowFloor),
      ("advisory.partialBelowFloor", testAdvisoryPartialBelowFloor),
      ("advisory.allCapped",       testAdvisoryAllCapped),
      ("advisory.truncatedAtCap",  testAdvisoryTruncatedAtCap),
      ("advisory.tooFewVerdictRows", testAdvisoryTooFewVerdictRows),
      ("advisory.ordinaryRunIsEmpty", testAdvisoryOrdinaryRunIsEmpty),
      ("fmtResult.belowFloorMark", testFmtResultBelowFloorMark),
      ("fmtResult.advisoryLines",  testFmtResultAdvisoryLines),
      ("custom.ladder",            testCustomLadder),
      ("custom.emptyRejected",     testCustomEmptyRejected),
      ("custom.duplicateRejected", testCustomDuplicateRejected),
      ("signalFloorMultiplier.validated", testSignalFloorMultiplierValidated) ]
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
    IO.println "advisory + custom-ladder tests passed"
  return anyFail

/-- Same compiled binary is parent and child; `_child` first arg
    distinguishes them for the custom-ladder run. -/
def main (args : List String) : IO UInt32 :=
  match args with
  | "_child" :: _ => LeanBench.Cli.dispatch args
  | _ => runTests
