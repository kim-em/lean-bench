import LeanBench

/-!
# Export and baseline comparison tests (issue #3)

Pure unit tests for the machine-readable export format and baseline
comparison logic. All fixtures are hand-built — no subprocess spawning
needed. Follows the same pattern as `LeanBenchTestFormat.lean`.

Coverage:

- `toJson` / `fromJson` round-trip for `BenchmarkResult`
- `toJson` / `fromJson` round-trip for `FixedResult`
- Top-level `ExportDocument` round-trip
- `export_schema_version` validation
- Baseline comparison: regression detection
- Baseline comparison: improvement detection
- Baseline comparison: stable (within threshold)
- Baseline comparison: no shared params
- Baseline comparison: fixed benchmark
- Baseline report formatting
-/

open LeanBench

/-- Three perfectly-linear points for testing. -/
private def linearPoints : Array DataPoint := #[
  { param := 2, innerRepeats := 4, totalNanos := 100
  , perCallNanos := 25.0, resultHash := some 0xabc
  , status := .ok, partOfVerdict := true },
  { param := 4, innerRepeats := 4, totalNanos := 200
  , perCallNanos := 50.0, resultHash := some 0xabc
  , status := .ok, partOfVerdict := true },
  { param := 8, innerRepeats := 4, totalNanos := 400
  , perCallNanos := 100.0, resultHash := some 0xabc
  , status := .ok, partOfVerdict := true } ]

private def sampleResult : BenchmarkResult :=
  { function := `Sample.linearFn
  , complexityFormula := "n"
  , hashable := true
  , config := {}
  , points := linearPoints
  , ratios := #[(2, 12.5), (4, 12.5), (8, 12.5)]
  , verdict := .consistentWithDeclaredComplexity
  , cMin? := some 12.5
  , cMax? := some 12.5
  , verdictDroppedLeading := 0
  , slope? := some 0.0
  , spawnFloorNanos? := some 5000000 }

private def sampleEnv : Env :=
  { leanVersion := "4.30.0-rc2"
  , leanToolchain := "leanprover/lean4:v4.30.0-rc2"
  , platformTarget := "x86_64-unknown-linux-gnu"
  , os := "linux"
  , arch := some "x86_64"
  , cpuModel := none
  , cpuCores := some 8
  , hostname := some "test-host"
  , exeName := "test-exe"
  , leanBenchVersion := "0.1.0"
  , gitCommit := some "deadbeefcafef00d1234567890abcdef12345678"
  , gitDirty := some false
  , timestampUnixMs := 1714316040000
  , timestampIso := "2026-04-28T12:34:00Z" }

private def sampleResultWithEnv : BenchmarkResult :=
  { sampleResult with env? := some sampleEnv }

private def sampleFixedResult : FixedResult :=
  { function := `Sample.cheap
  , hashable := true
  , config := { repeats := 3 }
  , points := #[
      { repeatIndex := 0, totalNanos := 1_000_000
      , resultHash := some 0xfeed, status := .ok },
      { repeatIndex := 1, totalNanos := 1_100_000
      , resultHash := some 0xfeed, status := .ok },
      { repeatIndex := 2, totalNanos := 1_050_000
      , resultHash := some 0xfeed, status := .ok } ]
  , medianNanos? := some 1_050_000
  , minNanos? := some 1_000_000
  , maxNanos? := some 1_100_000
  , hashesAgree := true }

/-! ## Round-trip tests -/

def testParametricRoundtrip : IO UInt32 := do
  let json := Export.benchmarkResultToJson sampleResultWithEnv
  match Export.benchmarkResultFromJson json with
  | .error e =>
    IO.eprintln s!"parametric round-trip parse error: {e}"
    return 1
  | .ok r =>
    -- Check key fields survived the round-trip
    let ok :=
      r.function == `Sample.linearFn &&
      r.complexityFormula == "n" &&
      r.hashable == true &&
      r.points.size == 3 &&
      r.ratios.size == 3 &&
      r.verdict == .consistentWithDeclaredComplexity &&
      r.cMin? == some 12.5 &&
      r.cMax? == some 12.5 &&
      r.slope? == some 0.0 &&
      r.verdictDroppedLeading == 0 &&
      r.spawnFloorNanos? == some 5000000 &&
      r.env?.isSome
    unless ok do
      IO.eprintln s!"parametric round-trip field mismatch: {repr r.function}, points={r.points.size}"
      return 1
    -- Check first data point
    let dp := r.points[0]!
    unless dp.param == 2 && dp.innerRepeats == 4 && dp.totalNanos == 100
        && dp.status == .ok && dp.resultHash == some 0xabc
        && dp.trialIndex == 0 do
      IO.eprintln s!"parametric round-trip point mismatch: {repr dp}"
      return 1
    -- Check env round-trip
    match r.env? with
    | some env =>
      unless env.leanVersion == "4.30.0-rc2" && env.os == "linux"
          && env.hostname == some "test-host" do
        IO.eprintln s!"env round-trip mismatch"
        return 1
    | none =>
      IO.eprintln "env missing after round-trip"
      return 1
    return 0

def testFixedRoundtrip : IO UInt32 := do
  let json := Export.fixedResultToJson sampleFixedResult
  match Export.fixedResultFromJson json with
  | .error e =>
    IO.eprintln s!"fixed round-trip parse error: {e}"
    return 1
  | .ok r =>
    let ok :=
      r.function == `Sample.cheap &&
      r.hashable == true &&
      r.points.size == 3 &&
      r.medianNanos? == some 1_050_000 &&
      r.minNanos? == some 1_000_000 &&
      r.maxNanos? == some 1_100_000 &&
      r.hashesAgree == true
    unless ok do
      IO.eprintln s!"fixed round-trip field mismatch"
      return 1
    return 0

def testDocumentRoundtrip : IO UInt32 := do
  let json := Export.toJson #[sampleResultWithEnv] #[sampleFixedResult] (some sampleEnv)
  let jsonStr := json.pretty
  match Lean.Json.parse jsonStr with
  | .error e =>
    IO.eprintln s!"document JSON re-parse failure: {e}"
    return 1
  | .ok reparsed =>
    -- Check top-level structure
    match reparsed.getObjValAs? Nat "export_schema_version" with
    | .ok v =>
      unless v == 1 do
        IO.eprintln s!"wrong export_schema_version: {v}"
        return 1
    | .error _ =>
      IO.eprintln "missing export_schema_version"
      return 1
    match reparsed.getObjVal? "results" with
    | .ok (.arr rs) =>
      -- Should have 2 results: 1 parametric + 1 fixed
      unless rs.size == 2 do
        IO.eprintln s!"expected 2 results, got {rs.size}"
        return 1
    | _ =>
      IO.eprintln "missing or invalid results array"
      return 1
    return 0

/-! ## Schema version validation -/

def testVersionRejection : IO UInt32 := do
  -- Future version should be rejected
  let futureJson := "{\"export_schema_version\": 99, \"results\": []}"
  match Lean.Json.parse futureJson with
  | .error _ =>
    IO.eprintln "JSON parse failure for version test"
    return 1
  | .ok j =>
    match Export.checkExportVersion j with
    | .ok () =>
      IO.eprintln "should have rejected export_schema_version=99"
      return 1
    | .error msg =>
      unless (msg.splitOn "newer").length > 1 do
        IO.eprintln s!"unexpected rejection message: {msg}"
        return 1
      return 0

def testVersionAcceptance : IO UInt32 := do
  let validJson := "{\"export_schema_version\": 1, \"results\": []}"
  match Lean.Json.parse validJson with
  | .error _ =>
    IO.eprintln "JSON parse failure for version test"
    return 1
  | .ok j =>
    match Export.checkExportVersion j with
    | .ok () => return 0
    | .error msg =>
      IO.eprintln s!"should have accepted version 1: {msg}"
      return 1

/-! ## Baseline comparison tests -/

/-- Build a variant of `sampleResult` with scaled per-call timings. -/
private def scaledResult (factor : Float) : BenchmarkResult :=
  let scaledPoints := linearPoints.map fun dp =>
    { dp with
        perCallNanos := dp.perCallNanos * factor
        totalNanos := (dp.totalNanos.toFloat * factor).toUInt64.toNat }
  { sampleResult with points := scaledPoints }

def testBaselineRegression : IO UInt32 := do
  -- Current is 50% slower than baseline -> regression
  let baseline := sampleResult
  let current := scaledResult 1.5
  let report := Export.compareParametric baseline current 10.0
  unless report.regressionCount == 3 do
    IO.eprintln s!"expected 3 regressions, got {report.regressionCount}"
    return 1
  unless report.improvementCount == 0 do
    IO.eprintln s!"expected 0 improvements, got {report.improvementCount}"
    return 1
  return 0

def testBaselineImprovement : IO UInt32 := do
  -- Current is 50% faster than baseline -> improvement
  let baseline := sampleResult
  let current := scaledResult 0.5
  let report := Export.compareParametric baseline current 10.0
  unless report.improvementCount == 3 do
    IO.eprintln s!"expected 3 improvements, got {report.improvementCount}"
    return 1
  unless report.regressionCount == 0 do
    IO.eprintln s!"expected 0 regressions, got {report.regressionCount}"
    return 1
  return 0

def testBaselineStable : IO UInt32 := do
  -- Current is 5% slower -> within 10% threshold -> stable
  let baseline := sampleResult
  let current := scaledResult 1.05
  let report := Export.compareParametric baseline current 10.0
  unless report.regressionCount == 0 && report.improvementCount == 0 do
    IO.eprintln s!"expected stable, got reg={report.regressionCount} imp={report.improvementCount}"
    return 1
  return 0

def testBaselineNoSharedParams : IO UInt32 := do
  -- Baseline has params 2,4,8; current has params 16,32,64
  let otherPoints : Array DataPoint := #[
    { param := 16, innerRepeats := 4, totalNanos := 800
    , perCallNanos := 200.0, resultHash := some 0xabc
    , status := .ok, partOfVerdict := true },
    { param := 32, innerRepeats := 4, totalNanos := 1600
    , perCallNanos := 400.0, resultHash := some 0xabc
    , status := .ok, partOfVerdict := true } ]
  let current : BenchmarkResult :=
    { sampleResult with points := otherPoints }
  let report := Export.compareParametric sampleResult current 10.0
  unless report.paramComparisons.isEmpty do
    IO.eprintln s!"expected empty comparisons, got {report.paramComparisons.size}"
    return 1
  return 0

def testBaselineFixed : IO UInt32 := do
  -- Fixed: current median is 2x baseline -> regression
  let baseline := sampleFixedResult
  let current : FixedResult :=
    { sampleFixedResult with
        medianNanos? := some 2_100_000
        minNanos? := some 2_000_000
        maxNanos? := some 2_200_000 }
  let report := Export.compareFixed baseline current 10.0
  unless report.regressionCount == 1 do
    IO.eprintln s!"expected 1 fixed regression, got {report.regressionCount}"
    return 1
  return 0

def testBaselineRunMultiple : IO UInt32 := do
  -- Test runBaseline matching by function name
  let base := sampleResult
  let cur := scaledResult 2.0
  let reports := Export.runBaseline #[base] #[] #[cur] #[] 10.0
  unless reports.size == 1 do
    IO.eprintln s!"expected 1 report, got {reports.size}"
    return 1
  unless reports[0]!.regressionCount == 3 do
    IO.eprintln s!"expected 3 regressions"
    return 1
  return 0

/-! ## Formatting test -/

def testBaselineReportFormat : IO UInt32 := do
  let baseline := sampleResult
  let current := scaledResult 1.5
  let report := Export.compareParametric baseline current 10.0
  let formatted := Export.fmtBaselineReport report
  -- Should contain key elements
  let needles := [
    "Sample.linearFn",
    "[parametric baseline comparison]",
    "[REGRESSION]",
    "param"
  ]
  for needle in needles do
    if !((formatted.splitOn needle).length > 1) then
      IO.eprintln s!"baseline format missing '{needle}' in:\n{formatted}"
      return 1
  return 0

def testBaselineReportsFormat : IO UInt32 := do
  let baseline := sampleResult
  let current := scaledResult 1.5
  let report := Export.compareParametric baseline current 10.0
  let formatted := Export.fmtBaselineReports #[report]
  unless (formatted.splitOn "REGRESSIONS DETECTED").length > 1 do
    IO.eprintln s!"summary format missing 'REGRESSIONS DETECTED' in:\n{formatted}"
    return 1
  return 0

/-! ## Driver -/

def main : IO UInt32 := do
  let mut anyFail : UInt32 := 0
  for (label, t) in
    [ ("parametricRoundtrip",     testParametricRoundtrip),
      ("fixedRoundtrip",          testFixedRoundtrip),
      ("documentRoundtrip",       testDocumentRoundtrip),
      ("versionRejection",        testVersionRejection),
      ("versionAcceptance",       testVersionAcceptance),
      ("baselineRegression",      testBaselineRegression),
      ("baselineImprovement",     testBaselineImprovement),
      ("baselineStable",          testBaselineStable),
      ("baselineNoSharedParams",  testBaselineNoSharedParams),
      ("baselineFixed",           testBaselineFixed),
      ("baselineRunMultiple",     testBaselineRunMultiple),
      ("baselineReportFormat",    testBaselineReportFormat),
      ("baselineReportsFormat",   testBaselineReportsFormat) ]
  do
    let code ← t
    if code == 0 then
      IO.println s!"  ok  {label}"
    else
      IO.eprintln s!"FAIL: {label}"
      anyFail := 1
  if anyFail == 0 then
    IO.println "export and baseline tests passed"
  return anyFail
