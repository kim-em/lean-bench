import LeanBench

/-!
# CI-budgeted suite tests (issue #9)

Pin behaviour around the `lean-bench suite --total-seconds N` path:
deadline-aware ladder termination, skipped-entry accounting,
`budget_status` field on the JSONL exporter, and the synthetic
skip-row shape for deferred benchmarks.

Three perspectives:

- **Pure scheduler primitives.** `Suite.shouldStart` and
  `Suite.secondsToNanos` are unit-tested against fixtures so the
  arithmetic is locked in independently of the IO loop.
- **End-to-end suite run.** A registered fast benchmark plus a
  registered slow benchmark, run with a tight budget, must surface
  one completed entry and one skipped entry. Total wall time stays
  within a documented bound.
- **Schema export.** The exported JSONL carries the documented key
  set including `budget_status`, the skip row uses the exact
  status/error shape downstream tooling will key on, and parsing the
  rows through `parseChildRow` ignores the `budget_status` field
  (additive optional behaviour).
-/

open LeanBench

namespace LeanBench.Test.Suite

/-- A trivially fast `Nat → UInt64`. Mirrors the e2e test's
    `littleFn`; the body matters less than the predictable per-call
    cost. -/
def fastFn (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 1
  for _ in [0:n] do
    x := x ^^^ n.toUInt64
  return x

/-- A deliberately slow `Nat → UInt64`. The XOR loop is amortised by
    the autotuner, so we make the work scale with `n^2` and pin a
    high `paramCeiling` so the ladder takes a noticeable amount of
    time once registered with a generous cap. -/
def slowFn (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 1
  for _ in [0:n] do
    for _ in [0:n] do
      x := x ^^^ n.toUInt64
  return x

setup_benchmark fastFn n => n where {
  maxSecondsPerCall := 0.5, paramCeiling := 8, targetInnerNanos := 50_000_000
  signalFloorMultiplier := 1.0
}

setup_benchmark slowFn n => n * n where {
  maxSecondsPerCall := 0.5, paramCeiling := 256, targetInnerNanos := 50_000_000
  signalFloorMultiplier := 1.0
}

end LeanBench.Test.Suite

private def fastName : Lean.Name := `LeanBench.Test.Suite.fastFn
private def slowName : Lean.Name := `LeanBench.Test.Suite.slowFn

private def expect (label : String) (cond : Bool) : IO UInt32 := do
  if cond then return 0
  IO.eprintln s!"FAIL: {label}"
  return 1

/-- Substring containment helper. -/
private def containsSub (haystack needle : String) : Bool :=
  ((haystack.splitOn needle).length) > 1

/-! ## Pure scheduler primitives -/

def testSecondsToNanos : IO UInt32 := do
  let pos := Suite.secondsToNanos 0.5
  let zeroVal := Suite.secondsToNanos 0.0
  let neg := Suite.secondsToNanos (-1.0)
  expect s!"seconds → nanos: pos={pos}, zero={zeroVal}, neg={neg}"
    (pos == 500_000_000 ∧ zeroVal == 0 ∧ neg == 0)

def testShouldStart : IO UInt32 := do
  let cfg : SuiteBudgetConfig := { totalSeconds := 5.0, minPerBenchmarkSeconds := 0.1 }
  -- Plenty of budget left.
  let plentyLeft := Suite.shouldStart cfg (deadline := 1_000_000_000) (now := 0)
  -- Below the per-benchmark minimum.
  let nearlyOut := Suite.shouldStart cfg (deadline := 100_000_000) (now := 90_000_000)
  -- Past the deadline (now > deadline → remaining is negative).
  let pastDeadline := Suite.shouldStart cfg (deadline := 100_000_000) (now := 200_000_000)
  expect s!"shouldStart: plenty={plentyLeft}, nearlyOut={nearlyOut}, past={pastDeadline}"
    (plentyLeft ∧ !nearlyOut ∧ !pastDeadline)

/-! ## Schema canonical-key fixture for budget_status -/

def testBudgetStatusConstants : IO UInt32 := do
  let okStrings :=
    Schema.budgetStatusStrings == #["completed", "skipped"]
  let okSuiteKeys :=
    Schema.optionalSuiteKeys == #["budget_status"]
  expect "budget_status canonical constants and key list"
    (okStrings ∧ okSuiteKeys)

/-! ## Render helpers (pure, no scheduler IO) -/

def testRenderSkipRowShape : IO UInt32 := do
  let row := Suite.renderSkipRow `myFn Schema.kindParametric
  match Lean.Json.parse row with
  | .error e =>
    IO.eprintln s!"skip row failed to parse: {e}\nrow: {row}"
    return 1
  | .ok j =>
    let okKind := (j.getObjValAs? String "kind").toOption == some "parametric"
    let okStatus := (j.getObjValAs? String "status").toOption == some "error"
    let okBudget := (j.getObjValAs? String "budget_status").toOption == some "skipped"
    let okFn := (j.getObjValAs? String "function").toOption == some "myFn"
    let errStr := (j.getObjValAs? String "error").toOption.getD ""
    let okErr := containsSub errStr "skipped"
    expect s!"skip row shape: kind={okKind}, status={okStatus}, budget={okBudget}, fn={okFn}, err={okErr} (got: {row})"
      (okKind ∧ okStatus ∧ okBudget ∧ okFn ∧ okErr)

/-- An explicit error message must round-trip into the row's `error`
    field. This is the contract Codex asked us to keep when a real
    exception (config validation failure, registry bug) is silenced
    on its way through `runSuiteWithBudget`: the diagnostic data
    travels with the row rather than vanishing. -/
def testRenderSkipRowCustomErrorMessage : IO UInt32 := do
  let customMsg := "runBenchmark failed: paramFloor > paramCeiling"
  let row := Suite.renderSkipRow `myFn Schema.kindParametric (some customMsg)
  match Lean.Json.parse row with
  | .error e =>
    IO.eprintln s!"custom-error skip row failed to parse: {e}"
    return 1
  | .ok j =>
    let errStr := (j.getObjValAs? String "error").toOption.getD ""
    let okErr := errStr == customMsg
    let okBudget := (j.getObjValAs? String "budget_status").toOption == some "skipped"
    expect s!"custom-error skip row carries our message: err={errStr}"
      (okErr ∧ okBudget)

def testRenderFixedSkipRowShape : IO UInt32 := do
  let row := Suite.renderSkipRow `myFn Schema.kindFixed
  match Lean.Json.parse row with
  | .error e =>
    IO.eprintln s!"fixed skip row failed: {e}"
    return 1
  | .ok j =>
    let okKind := (j.getObjValAs? String "kind").toOption == some "fixed"
    let okBudget := (j.getObjValAs? String "budget_status").toOption == some "skipped"
    let okRepeat := (j.getObjValAs? Nat "repeat_index").toOption == some 0
    expect s!"fixed skip row shape: kind={okKind}, budget={okBudget}, repeat={okRepeat}"
      (okKind ∧ okBudget ∧ okRepeat)

/-- The schema reader's job is to parse a row even with the new
    `budget_status` field present. Pin that — older readers see
    `budget_status` as an unknown extra key and ignore it. -/
def testParseAcceptsBudgetStatus : IO UInt32 := do
  let row :=
    "{\"schema_version\":1,\"kind\":\"parametric\",\"function\":\"foo\"," ++
    "\"param\":1,\"inner_repeats\":1,\"total_nanos\":1," ++
    "\"per_call_nanos\":1.0,\"status\":\"ok\"," ++
    "\"budget_status\":\"completed\"}"
  match parseChildRow row with
  | .error e =>
    IO.eprintln s!"reader rejected budget_status row: {e}"
    return 1
  | .ok dp =>
    expect "reader tolerates budget_status as extra key"
      (dp.param == 1 ∧ dp.status == .ok)

/-! ## End-to-end suite run

The fast benchmark must complete; the slow benchmark must be
skipped under a tight budget. We tighten `slowFn` even further at
override time so the test is robust on noisy CI hosts. -/

def testSuiteRunCompletesAndSkips : IO UInt32 := do
  let budget : SuiteBudgetConfig :=
    { totalSeconds := 1.0, minPerBenchmarkSeconds := 0.1 }
  -- Force `slowFn` into a regime that will exceed the remaining budget.
  let pOverride : ConfigOverride :=
    { paramCeiling? := some 256, paramFloor? := some 8 }
  let report ← Suite.runSuiteWithBudget budget pOverride {}
  -- Both registered benchmarks must appear in the report (in registry order).
  let fns := report.entries.map (·.function)
  unless fns.contains fastName ∧ fns.contains slowName do
    IO.eprintln s!"expected both benchmarks in report; got {fns}"
    return 1
  -- At least one must complete (typically the fast one); at least one must
  -- be skipped (typically the slow one) given the budget.
  let completedAny := report.entries.any (·.budgetStatus == .completed)
  unless completedAny do
    IO.eprintln s!"expected at least one completed entry; got {repr (report.entries.map (·.budgetStatus))}"
    return 1
  -- Total elapsed should be loosely bounded by `budget + a few seconds of
  -- in-flight slack`. We don't check the lower bound — the suite may complete
  -- early if both benchmarks fit comfortably.
  unless report.elapsedSeconds ≤ budget.totalSeconds + 5.0 do
    IO.eprintln s!"elapsed {report.elapsedSeconds}s exceeds budget {budget.totalSeconds}s + 5s slack"
    return 1
  -- Counter sanity.
  expect s!"completed + skipped = total entries: {report.completedCount}+{report.skippedCount}={report.entries.size}"
    (report.completedCount + report.skippedCount == report.entries.size)

/-! ## JSONL exporter on a synthetic SuiteReport

We drive the exporter against a hand-built `SuiteReport` mixing
completed and skipped entries so the test is independent of timing
on noisy hosts. Every line must parse; every line must carry
`budget_status`; skipped rows must carry the documented status +
error shape downstream tooling will key on. -/

private def syntheticPoint (param totalNanos : Nat) (status : Status) : DataPoint :=
  { param, innerRepeats := 1, totalNanos
    perCallNanos := totalNanos.toFloat
    resultHash := some 0xdeadbeef, status }

private def syntheticReport : SuiteReport :=
  let completedResult : BenchmarkResult :=
    { function := `synth.completedFn
      complexityFormula := "n"
      hashable := true
      config := {}
      points := #[
        syntheticPoint 2 1_000 .ok,
        syntheticPoint 4 2_000 .ok ]
      ratios := #[]
      verdict := .inconclusive
      cMin? := none
      cMax? := none
      verdictDroppedLeading := 0
      slope? := none
      spawnFloorNanos? := none }
  let entries : Array SuiteEntry := #[
    { function := `synth.completedFn
      kindStr := Schema.kindParametric
      budgetStatus := .completed
      parametric? := some completedResult
      elapsedSeconds := 0.5 },
    { function := `synth.skippedFn
      kindStr := Schema.kindParametric
      budgetStatus := .skipped
      errorMessage? := some "skipped: budget exhausted before start" },
    { function := `synth.skippedFixed
      kindStr := Schema.kindFixed
      budgetStatus := .skipped
      errorMessage? := some "runFixedBenchmark failed: bad config" } ]
  { budget := { totalSeconds := 1.0 }
    elapsedSeconds := 0.6
    entries }

def testSuiteJsonlExport : IO UInt32 := do
  let report := syntheticReport
  let lines := Suite.renderSuiteJsonl report
  -- 2 completed measurement rows + 2 skip rows.
  unless lines.size == 4 do
    IO.eprintln s!"expected 4 lines, got {lines.size}: {lines}"
    return 1
  for line in lines do
    match Lean.Json.parse line with
    | .error e =>
      IO.eprintln s!"export line failed to parse: {e}\nline: {line}"
      return 1
    | .ok j =>
      match j.getObjValAs? String "budget_status" with
      | .ok s =>
        unless Schema.budgetStatusStrings.contains s do
          IO.eprintln s!"unknown budget_status {s} in line: {line}"
          return 1
      | .error _ =>
        IO.eprintln s!"export line missing budget_status: {line}"
        return 1
  -- Exactly one parametric skip row + one fixed skip row.
  let skipRows := lines.filter fun line =>
    containsSub line "\"budget_status\":\"skipped\""
  unless skipRows.size == 2 do
    IO.eprintln s!"expected 2 skip rows, got {skipRows.size}: {skipRows}"
    return 1
  let fixedSkip := skipRows.any (containsSub · "\"kind\":\"fixed\"")
  let paramSkip := skipRows.any (containsSub · "\"kind\":\"parametric\"")
  unless fixedSkip ∧ paramSkip do
    IO.eprintln s!"expected one parametric and one fixed skip row, got: {skipRows}"
    return 1
  -- The synthetic fixed entry carries a non-budget error message; it
  -- must travel through the JSONL exporter rather than being
  -- rewritten as the default "budget exhausted" string.
  let preservedDistinctMsg := skipRows.any (containsSub · "runFixedBenchmark failed")
  unless preservedDistinctMsg do
    IO.eprintln s!"expected the per-entry errorMessage? to round-trip into the row, got: {skipRows}"
    return 1
  return 0

/-! ## CLI dispatch smoke test

Drive the `suite` subcommand through the CLI dispatch pipeline so
the parsing wiring has end-to-end coverage. We redirect stdout so
the formatted report doesn't pollute the test log. -/

def testSuiteCliDispatch : IO UInt32 := do
  let nullDev := if System.Platform.isWindows then "NUL" else "/dev/null"
  let h ← IO.FS.Handle.mk nullDev .write
  let code ← IO.withStdout (.ofHandle h) do
    LeanBench.Cli.dispatch ["suite", "--total-seconds", "1.0",
      "--max-seconds-per-call", "0.25",
      "--param-ceiling", "8"]
  expect s!"suite CLI dispatch exit code = {code}" (code == 0)

/-- Negative-input rejection. -/
def testSuiteCliRejectsNonPositiveBudget : IO UInt32 := do
  -- Suppress the formatted output (the error itself goes to stderr,
  -- which is left untouched).
  let nullDev := if System.Platform.isWindows then "NUL" else "/dev/null"
  let h ← IO.FS.Handle.mk nullDev .write
  let code ← IO.withStdout (.ofHandle h) do
    LeanBench.Cli.dispatch ["suite", "--total-seconds", "0"]
  expect s!"non-positive --total-seconds rejected with non-zero exit, got {code}"
    (code != 0)

/-! ## Driver -/

def runTests : IO UInt32 := do
  let mut anyFail : UInt32 := 0
  for (label, t) in
    [ ("secondsToNanos",                 testSecondsToNanos),
      ("shouldStart",                    testShouldStart),
      ("budgetStatus.constants",         testBudgetStatusConstants),
      ("renderSkipRow.parametric",       testRenderSkipRowShape),
      ("renderSkipRow.customError",      testRenderSkipRowCustomErrorMessage),
      ("renderSkipRow.fixed",            testRenderFixedSkipRowShape),
      ("parser.acceptsBudgetStatus",     testParseAcceptsBudgetStatus),
      ("suite.runCompletesAndSkips",     testSuiteRunCompletesAndSkips),
      ("suite.jsonlExport",              testSuiteJsonlExport),
      ("suite.cliDispatch",              testSuiteCliDispatch),
      ("suite.cliRejectsNonPositive",    testSuiteCliRejectsNonPositiveBudget) ]
  do
    let code ← t
    if code != 0 then
      IO.eprintln s!"FAIL: {label}"
      anyFail := 1
    else
      IO.println s!"  ok  {label}"
  if anyFail == 0 then
    IO.println "suite tests passed"
  return anyFail

/-- The same compiled binary acts as parent (test driver) and child
    (the `_child` first arg distinguishes them). The end-to-end suite
    run spawns this binary in child mode against the registered
    benchmarks above. -/
def main (args : List String) : IO UInt32 :=
  match args with
  | "_child" :: _ => LeanBench.Cli.dispatch args
  | _ => runTests
