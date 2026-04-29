import LeanBench

/-!
# Issue #47: `run` exits non-zero on zero-verdict-eligible-rows runs

The repro from the issue: a parametric `run` whose schedule lands every
rung below the per-spawn signal floor still emits a "verdict:
inconclusive" report and exits 0, masking a calibration bug. The fix is
a dedicated exit code (`exitNoUsableData = 2`) that surfaces the
condition to scripts and CI without parsing stdout.

What's pinned:

- Pure unit test: `BenchmarkResult.noUsableData` returns `true` exactly
  when `ratios` is empty.
- Pure unit test: a hand-built `BenchmarkResult` with
  `belowSignalFloor` advisory and empty `ratios` is classified as
  no-usable-data.
- Integration test: `Cli.dispatch ["run", FAST_BENCH]` against a
  benchmark whose `signalFloorMultiplier` is large enough to flag
  every row exits `exitNoUsableData` (= 2).
- Integration test: `Cli.dispatch ["compare", A, B]` propagates the
  same exit code when at least one constituent has no usable data.
- Negative control: a normal benchmark (`littleFnNormal`) exits 0 — so
  the test isn't passing because every run trips the new code path.
-/

open LeanBench

private def expectEq {α} [BEq α] [Repr α] (label : String) (got want : α) :
    IO Unit := do
  unless got == want do
    throw <| .userError
      s!"{label}: expected {repr want}, got {repr got}"

namespace LeanBench.Test.NoUsableData

/-! ## Pure unit tests on the `noUsableData` predicate -/

private def mkResult (ratios : Array Ratio) (advisories : Array Advisory) :
    BenchmarkResult :=
  { function := `Sample.fast
  , complexityFormula := "n"
  , hashable := true
  , config := {}
  , points := #[]
  , ratios
  , verdict := .inconclusive
  , cMin? := none
  , cMax? := none
  , verdictDroppedLeading := 0
  , slope? := none
  , spawnFloorNanos? := some 1
  , advisories }

def testPredicateEmptyRatios : IO UInt32 := do
  let r := mkResult #[] #[.belowSignalFloor]
  expectEq "noUsableData.allBelowFloor" r.noUsableData true
  return 0

def testPredicateAllCappedNoOk : IO UInt32 := do
  let r := mkResult #[] #[.allCapped]
  expectEq "noUsableData.allCapped" r.noUsableData true
  return 0

def testPredicateNonEmptyRatios : IO UInt32 := do
  let r := mkResult #[(4, 25.0), (8, 25.0), (16, 25.0)] #[]
  expectEq "noUsableData.healthy" r.noUsableData false
  return 0

def testPredicateOneRatioStillNoData : IO UInt32 := do
  -- A run with a single below-resolution row but `ratios = #[]`
  -- (warmup-trim + floor filter wiped everything out) is still
  -- classified as no-usable-data. With a single surviving row in
  -- `ratios`, however, the run produced *some* data — the slope fit
  -- can't proceed, but issue #47 explicitly scopes the new exit code
  -- to "zero verdict-eligible rows."
  let r1 := mkResult #[] #[.tooFewVerdictRows 0]
  expectEq "noUsableData.tooFew0" r1.noUsableData true
  let r2 := mkResult #[(4, 25.0)] #[.tooFewVerdictRows 1]
  expectEq "noUsableData.tooFew1" r2.noUsableData false
  return 0

def testPredicateAllErrored : IO UInt32 := do
  -- The harness can hit "no usable data" via several disjoint paths:
  -- below-floor, all-capped, all-errored. The predicate sees them
  -- uniformly through `ratios.isEmpty`. Pin the all-errored case
  -- explicitly so a future regression in advisory composition can't
  -- silently break it.
  let r := mkResult #[] #[]
  expectEq "noUsableData.allErrored" r.noUsableData true
  return 0

def testPredicateBudgetTruncatedExcluded : IO UInt32 := do
  -- Budget-truncated runs (`--total-seconds`, issue #9) are user-
  -- requested early stops, not calibration bugs — the harness would
  -- have produced data given more time. Don't surface them via the
  -- new exit code; the budget summary is the right signal.
  let r := { mkResult #[] #[.belowSignalFloor] with budgetTruncated := true }
  expectEq "noUsableData.budgetTruncated" r.noUsableData false
  return 0

/-! ## Integration: `run` against a benchmark with no usable data -/

/-- Trivial XOR loop — same shape as the e2e test's `littleFn`. -/
def fastFn (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 1
  for _ in [0:n] do
    x := x ^^^ n.toUInt64
  return x

/-- Identical body — used to exercise the `compare` propagation path. -/
def fastFnTwin (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 1
  for _ in [0:n] do
    x := x ^^^ n.toUInt64
  return x

-- `signalFloorMultiplier := 1.0e9` blows the per-spawn floor up so far
-- above any real measurement that every row gets flagged
-- `belowSignalFloor`, `ratios` ends up empty, and the run is classified
-- `noUsableData`. The schedule is small / the cap is short so the test
-- runs in ~1s on CI.
setup_benchmark fastFn n => n where {
  maxSecondsPerCall := 0.5
  paramCeiling := 8
  targetInnerNanos := 50_000_000
  signalFloorMultiplier := 1.0e9
}

setup_benchmark fastFnTwin n => n where {
  maxSecondsPerCall := 0.5
  paramCeiling := 8
  targetInnerNanos := 50_000_000
  signalFloorMultiplier := 1.0e9
}

-- A second registration on the same `fastFn` body but with the floor
-- filter disabled, so we can negative-control the exit-code change
-- against an ordinary run.
def normalFn (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 1
  for _ in [0:n] do
    x := x ^^^ n.toUInt64
  return x

setup_benchmark normalFn n => n where {
  maxSecondsPerCall := 0.5
  paramCeiling := 8
  targetInnerNanos := 50_000_000
  signalFloorMultiplier := 1.0
}

private def fastName : String := "LeanBench.Test.NoUsableData.fastFn"
private def fastTwinName : String := "LeanBench.Test.NoUsableData.fastFnTwin"
private def normalName : String := "LeanBench.Test.NoUsableData.normalFn"

/-- `run NAME` against a benchmark with every rung below floor returns
    `exitNoUsableData` (= 2), distinguishing it from `0` (success) and
    from `1` (baseline regression / handler error). -/
def testRunBelowFloorExitsTwo : IO UInt32 := do
  let code ← LeanBench.Cli.dispatch ["run", fastName]
  if code == LeanBench.exitNoUsableData then
    IO.println "  ok  run.belowFloor.exitsTwo"
    return 0
  IO.eprintln s!"FAIL: run.belowFloor.exitsTwo: expected exit {LeanBench.exitNoUsableData}, got {code}"
  return 1

/-- A normal benchmark (floor filter disabled) keeps exiting 0. -/
def testRunNormalStillZero : IO UInt32 := do
  let code ← LeanBench.Cli.dispatch ["run", normalName]
  if code == 0 then
    IO.println "  ok  run.normal.exitsZero"
    return 0
  IO.eprintln s!"FAIL: run.normal.exitsZero: expected exit 0, got {code}"
  return 1

/-- `compare` with at least one constituent in the no-usable-data
    state propagates `exitNoUsableData`. -/
def testCompareBelowFloorPropagates : IO UInt32 := do
  let code ← LeanBench.Cli.dispatch ["compare", fastName, fastTwinName]
  if code == LeanBench.exitNoUsableData then
    IO.println "  ok  compare.belowFloor.exitsTwo"
    return 0
  IO.eprintln s!"FAIL: compare.belowFloor.exitsTwo: expected exit {LeanBench.exitNoUsableData}, got {code}"
  return 1

/-! ## Driver -/

def runTests : IO UInt32 := do
  let mut anyFail : UInt32 := 0
  for (label, t) in
    [ ("predicate.emptyRatios",      testPredicateEmptyRatios),
      ("predicate.allCappedNoOk",    testPredicateAllCappedNoOk),
      ("predicate.nonEmptyRatios",   testPredicateNonEmptyRatios),
      ("predicate.oneRatioStillNoData", testPredicateOneRatioStillNoData),
      ("predicate.allErrored",         testPredicateAllErrored),
      ("predicate.budgetTruncatedExcluded", testPredicateBudgetTruncatedExcluded),
      ("run.belowFloor.exitsTwo",    testRunBelowFloorExitsTwo),
      ("run.normal.exitsZero",       testRunNormalStillZero),
      ("compare.belowFloor.exitsTwo", testCompareBelowFloorPropagates) ]
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
    IO.println "no-usable-data exit-code tests passed"
  return anyFail

end LeanBench.Test.NoUsableData

/-- Same compiled binary is parent and child; `_child` first arg
    distinguishes them for the spawned ladder rungs. -/
def main (args : List String) : IO UInt32 :=
  match args with
  | "_child" :: _ => LeanBench.Cli.dispatch args
  | _ => LeanBench.Test.NoUsableData.runTests
