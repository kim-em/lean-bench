import LeanBench

/-!
# Formatting regression test (issue #2)

Pin the human-readable output of `Format.*` against fixed inputs so
report layout changes are intentional. The `BenchmarkResult` /
`ComparisonReport` literals constructed below are entirely pure (no
subprocesses), so the test is fast and deterministic across hosts.

When a layout change is intentional, run this test, copy the printed
"actual" block over the "expected" string in the test, commit, and
mention the format change in the PR description.

Coverage:

- `fmtNanos`               (5 magnitude tiers)
- `fmtNanosStr`            (combined number+unit)
- `fmtSpec`                (registered parametric benchmark)
- `fmtFixedSpec`           (registered fixed benchmark)
- `fmtResult`              (multi-row parametric table, with verdict)
- `fmtComparison`          (multi-result parametric report)
- `fmtFixedResult`         (multi-row fixed table)
- `fmtFixedComparison`     (multi-result fixed report)
- `fmtVerifyReport`        (passing AND failing)
- `fmtVerify`              (multi-report summary)
- `fmtCombinedVerify`      (parametric + fixed)
-/

open LeanBench

/-- Snapshot equality. On failure prints both expected and actual
    side-by-side so the diff is visible in CI logs. -/
private def snapshot (label expected actual : String) : IO UInt32 := do
  if actual == expected then
    IO.println s!"  ok  {label}"
    return 0
  IO.eprintln s!"FAIL: {label}"
  IO.eprintln "----- expected -----"
  IO.eprintln expected
  IO.eprintln "----- actual   -----"
  IO.eprintln actual
  IO.eprintln "--------------------"
  return 1

/-! ## Fixed inputs -/

private def sampleSpec : BenchmarkSpec :=
  { name := `Sample.linearFn
  , complexityName := `Sample.linearFn._leanBench_complexity
  , complexityFormula := "n"
  , runCheckedName := `Sample.linearFn._leanBench_runChecked
  , hashable := true
  , config := {} }

private def sampleNoHashSpec : BenchmarkSpec :=
  { sampleSpec with name := `Sample.noHashFn, hashable := false }

private def sampleFixedSpec : FixedSpec :=
  { name := `Sample.cheap
  , runnerName := `Sample.cheap._leanBench_runner
  , hashable := true
  , config := { repeats := 3 } }

/-- Three perfectly-linear points so the verdict is unambiguous. -/
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
  { function := sampleSpec.name
  , complexityFormula := sampleSpec.complexityFormula
  , hashable := true
  , config := sampleSpec.config
  , points := linearPoints
  , ratios := #[(2, 12.5), (4, 12.5), (8, 12.5)]
  , verdict := .consistentWithDeclaredComplexity
  , cMin? := some 12.5
  , cMax? := some 12.5
  , verdictDroppedLeading := 0
  , slope? := some 0.0
  , spawnFloorNanos? := none }

/-- A second result for the comparison snapshot. Hand-tuned so the
    table is not identical to `sampleResult`. -/
private def sampleResult2 : BenchmarkResult :=
  { function := `Sample.otherFn
  , complexityFormula := "n"
  , hashable := true
  , config := {}
  , points := #[
      { param := 2, innerRepeats := 4, totalNanos := 200
      , perCallNanos := 50.0, resultHash := some 0xdef
      , status := .ok, partOfVerdict := true },
      { param := 4, innerRepeats := 4, totalNanos := 400
      , perCallNanos := 100.0, resultHash := some 0xdef
      , status := .ok, partOfVerdict := true },
      { param := 8, innerRepeats := 4, totalNanos := 800
      , perCallNanos := 200.0, resultHash := some 0xdef
      , status := .ok, partOfVerdict := true } ]
  , ratios := #[(2, 25.0), (4, 25.0), (8, 25.0)]
  , verdict := .consistentWithDeclaredComplexity
  , cMin? := some 25.0
  , cMax? := some 25.0
  , verdictDroppedLeading := 0
  , slope? := some 0.0
  , spawnFloorNanos? := none }

private def sampleDivergence : DivergenceDetail :=
  { param := 2
  , hashes     := #[(`Sample.linearFn, some 0xabc), (`Sample.otherFn, some 0xdef)]
  , dissenters := #[`Sample.otherFn] }

private def sampleComparison : ComparisonReport :=
  { results := #[sampleResult, sampleResult2]
  , commonParams := #[2, 4, 8]
  , agreeOnCommon := .divergedAt #[sampleDivergence] }

/-- Multi-divergence variant: same earliest-diverging param 2 plus
    follow-on divergences at 4 and 8, used to pin the
    `also diverged at:` line. -/
private def sampleComparisonMulti : ComparisonReport :=
  { sampleComparison with
    agreeOnCommon := .divergedAt #[
      sampleDivergence,
      { sampleDivergence with param := 4 },
      { sampleDivergence with param := 8 } ] }

/-- Hash-unavailable variant: one of the compared functions has no
    Hashable instance, so we can't check agreement at all. -/
private def sampleComparisonHashUnavailable : ComparisonReport :=
  { sampleComparison with
    agreeOnCommon := .hashUnavailable #[`Sample.otherFn] }

private def fixedPoints : Array FixedDataPoint := #[
  { repeatIndex := 0, totalNanos := 1_000_000
  , resultHash := some 0xfeed, status := .ok },
  { repeatIndex := 1, totalNanos := 1_100_000
  , resultHash := some 0xfeed, status := .ok },
  { repeatIndex := 2, totalNanos := 1_050_000
  , resultHash := some 0xfeed, status := .ok } ]

private def sampleFixedResult : FixedResult :=
  { function := sampleFixedSpec.name
  , hashable := true
  , config := sampleFixedSpec.config
  , points := fixedPoints
  , medianNanos? := some 1_050_000
  , minNanos? := some 1_000_000
  , maxNanos? := some 1_100_000
  , hashesAgree := true }

private def sampleFixedComparison : FixedComparisonReport :=
  { results := #[sampleFixedResult]
  , agreeOnHash := .allAgreed }

/-! ## Snapshots

Each test calls `snapshot label expected actual`. To accept a
deliberate format change, replace the `expected` string with what
the test prints under "actual ----" and commit. -/

def testFmtNanos : IO UInt32 := do
  -- Each tier of the magnitude switch.
  let cases : List (Nat × String × String) :=
    [(123, "123.000", "ns"),
     (1_500, "1.500", "µs"),
     (2_500_000, "2.500", "ms"),
     (3_000_000_000, "3.000", "s"),
     (0, "0.000", "ns")]
  for (n, num, unit) in cases do
    let (gotNum, gotUnit) := Format.fmtNanos n
    if gotNum != num || gotUnit != unit then
      IO.eprintln s!"fmtNanos {n}: expected ({num}, {unit}), got ({gotNum}, {gotUnit})"
      return 1
  return 0

def testFmtNanosStr : IO UInt32 := do
  if Format.fmtNanosStr 1_500 != "1.500 µs" then
    IO.eprintln s!"fmtNanosStr 1500: expected '1.500 µs', got '{Format.fmtNanosStr 1500}'"
    return 1
  return 0

def testFmtSpec : IO UInt32 := do
  let expected := "  Sample.linearFn    expected complexity: n"
  snapshot "fmtSpec.hashable" expected (Format.fmtSpec sampleSpec)

def testFmtSpecNoHash : IO UInt32 := do
  let expected := "  Sample.noHashFn    expected complexity: n  (no Hashable)"
  snapshot "fmtSpec.noHashable" expected (Format.fmtSpec sampleNoHashSpec)

def testFmtFixedSpec : IO UInt32 := do
  let expected := "  Sample.cheap    [fixed] repeats=3"
  snapshot "fmtFixedSpec" expected (Format.fmtFixedSpec sampleFixedSpec)

def testFmtResult : IO UInt32 := do
  let expected :=
    "Sample.linearFn    expected complexity: n    [warm cache]\n" ++
    "  param  per-call    repeats  C\n" ++
    "      2   25.000 ns  ×2^2     C=12.500\n" ++
    "      4   50.000 ns  ×2^2     C=12.500\n" ++
    "      8  100.000 ns  ×2^2     C=12.500\n" ++
    "  verdict: consistent with declared complexity (cMin=12.500, cMax=12.500, β=+0.000)"
  snapshot "fmtResult" expected (Format.fmtResult sampleResult)

/-- Cold-mode snapshot. Exercises the same code path as `fmtResult`
    above, but with the `[cold cache]` tag swapped in. Pinned so a
    silent regression that drops the cold tag (or accidentally tags
    every result as warm) breaks here. -/
def testFmtResultCold : IO UInt32 := do
  let coldResult : BenchmarkResult :=
    { sampleResult with config := { sampleResult.config with cacheMode := .cold } }
  let expected :=
    "Sample.linearFn    expected complexity: n    [cold cache]\n" ++
    "  param  per-call    repeats  C\n" ++
    "      2   25.000 ns  ×2^2     C=12.500\n" ++
    "      4   50.000 ns  ×2^2     C=12.500\n" ++
    "      8  100.000 ns  ×2^2     C=12.500\n" ++
    "  verdict: consistent with declared complexity (cMin=12.500, cMax=12.500, β=+0.000)"
  snapshot "fmtResult.cold" expected (Format.fmtResult coldResult)

def testFmtComparison : IO UInt32 := do
  let expected :=
    "Sample.linearFn    expected complexity: n    [warm cache]\n" ++
    "  param  per-call    repeats  C\n" ++
    "      2   25.000 ns  ×2^2     C=12.500\n" ++
    "      4   50.000 ns  ×2^2     C=12.500\n" ++
    "      8  100.000 ns  ×2^2     C=12.500\n" ++
    "  verdict: consistent with declared complexity (cMin=12.500, cMax=12.500, β=+0.000)\n" ++
    "\n" ++
    "Sample.otherFn    expected complexity: n    [warm cache]\n" ++
    "  param  per-call    repeats  C\n" ++
    "      2   50.000 ns  ×2^2     C=25.000\n" ++
    "      4  100.000 ns  ×2^2     C=25.000\n" ++
    "      8  200.000 ns  ×2^2     C=25.000\n" ++
    "  verdict: consistent with declared complexity (cMin=25.000, cMax=25.000, β=+0.000)\n" ++
    "\n" ++
    "common params (apples-to-apples): 2, 4, 8\n" ++
    "agreement: DIVERGED on 1 param — earliest divergence at param=2:\n" ++
    "    Sample.linearFn  hash=0xabc    (baseline)\n" ++
    "    Sample.otherFn   hash=0xdef    differs from Sample.linearFn\n" ++
    "  (only result hashes are available; see doc/quickstart.md for what to expect)"
  snapshot "fmtComparison" expected (Format.fmtComparison sampleComparison)

/-- Multi-divergence: the earliest diverging param is highlighted
    with a hash table; later params land on a compact summary line. -/
def testFmtComparisonMulti : IO UInt32 := do
  let rendered := Format.fmtComparison sampleComparisonMulti
  let needles : List String := [
    "DIVERGED on 3 params — earliest divergence at param=2:",
    "Sample.linearFn  hash=0xabc    (baseline)",
    "Sample.otherFn   hash=0xdef    differs from Sample.linearFn",
    "also diverged at: 4, 8" ]
  for needle in needles do
    if !((rendered.splitOn needle).length > 1) then
      IO.eprintln s!"FAIL fmtComparison.multi: missing '{needle}' in:\n{rendered}"
      return 1
  return 0

/-- Hash-unavailable: the report names the offending function and
    points the user at the fix. -/
def testFmtComparisonHashUnavailable : IO UInt32 := do
  let rendered := Format.fmtComparison sampleComparisonHashUnavailable
  let needles : List String := [
    "agreement: cannot check — no Hashable instance for: Sample.otherFn",
    "register a Hashable instance" ]
  for needle in needles do
    if !((rendered.splitOn needle).length > 1) then
      IO.eprintln s!"FAIL fmtComparison.hashUnavailable: missing '{needle}' in:\n{rendered}"
      return 1
  return 0

def testFmtFixedResult : IO UInt32 := do
  let expected :=
    "Sample.cheap    [fixed] repeats=3\n" ++
    "  repeat 0  1.000 ms\n" ++
    "  repeat 1  1.100 ms\n" ++
    "  repeat 2  1.050 ms\n" ++
    "  median: 1.050 ms    min: 1.000 ms    max: 1.100 ms\n" ++
    "  hash: all repeats agree"
  snapshot "fmtFixedResult" expected (Format.fmtFixedResult sampleFixedResult)

def testFmtFixedComparison : IO UInt32 := do
  let expected :=
    "Sample.cheap    [fixed] repeats=3\n" ++
    "  repeat 0  1.000 ms\n" ++
    "  repeat 1  1.100 ms\n" ++
    "  repeat 2  1.050 ms\n" ++
    "  median: 1.050 ms    min: 1.000 ms    max: 1.100 ms\n" ++
    "  hash: all repeats agree\n" ++
    "\n" ++
    "relative median (baseline = Sample.cheap, the first CLI argument):\n" ++
    "  Sample.cheap: 1.000× (1.050 ms)\n" ++
    "agreement: all functions agree on output"
  snapshot "fmtFixedComparison" expected
    (Format.fmtFixedComparison sampleFixedComparison)

def testFmtVerifyPass : IO UInt32 := do
  let r : VerifyReport :=
    { spec := sampleSpec, checks := #[] }
  snapshot "fmtVerifyReport.pass"
    "  [ok ] Sample.linearFn"
    (Format.fmtVerifyReport r)

def testFmtVerifyFail : IO UInt32 := do
  let failingCheck : VerifyCheck :=
    { param := 0
    , point :=
        { param := 0, innerRepeats := 0, totalNanos := 0
        , perCallNanos := 0.0, resultHash := none
        , status := .error "boom" }
    , failure := some "child error at param=0: boom" }
  let r : VerifyReport :=
    { spec := sampleSpec, checks := #[failingCheck] }
  let expected :=
    "  [FAIL] Sample.linearFn\n" ++
    "         —  child error at param=0: boom"
  snapshot "fmtVerifyReport.fail" expected (Format.fmtVerifyReport r)

def testFmtVerifyMulti : IO UInt32 := do
  let pass : VerifyReport :=
    { spec := sampleSpec, checks := #[] }
  let fail : VerifyReport :=
    { spec := { sampleSpec with name := `Sample.brokenFn }
    , checks := #[
        { param := 1, point :=
            { param := 1, innerRepeats := 0, totalNanos := 0
            , perCallNanos := 0.0, resultHash := none
            , status := .error "kapow" }
        , failure := some "child error at param=1: kapow" } ] }
  let expected :=
    "verifying 2 benchmark(s)...\n" ++
    "  [ok ] Sample.linearFn\n" ++
    "  [FAIL] Sample.brokenFn\n" ++
    "         —  child error at param=1: kapow\n" ++
    "1 of 2 benchmark(s) failed verification"
  snapshot "fmtVerify.multi" expected (Format.fmtVerify #[pass, fail])

def testFmtCombinedVerify : IO UInt32 := do
  let pParam : VerifyReport := { spec := sampleSpec, checks := #[] }
  let fFixed : FixedVerifyReport :=
    { spec := sampleFixedSpec, checks := #[] }
  let combined : CombinedVerifyReports :=
    { parametric := #[pParam], fixed := #[fFixed] }
  let expected :=
    "verifying 2 benchmark(s)...\n" ++
    "  [ok ] Sample.linearFn\n" ++
    "  [ok ] Sample.cheap  [fixed]\n" ++
    "all 2 benchmark(s) passed"
  snapshot "fmtCombinedVerify" expected (Format.fmtCombinedVerify combined)

/-! ## Driver -/

def main : IO UInt32 := do
  let mut anyFail : UInt32 := 0
  for (label, t) in
    [ ("fmtNanos", testFmtNanos),
      ("fmtNanosStr", testFmtNanosStr),
      ("fmtSpec.hashable", testFmtSpec),
      ("fmtSpec.noHash", testFmtSpecNoHash),
      ("fmtFixedSpec", testFmtFixedSpec),
      ("fmtResult", testFmtResult),
      ("fmtResult.cold", testFmtResultCold),
      ("fmtComparison", testFmtComparison),
      ("fmtComparison.multi", testFmtComparisonMulti),
      ("fmtComparison.hashUnavailable", testFmtComparisonHashUnavailable),
      ("fmtFixedResult", testFmtFixedResult),
      ("fmtFixedComparison", testFmtFixedComparison),
      ("fmtVerifyReport.pass", testFmtVerifyPass),
      ("fmtVerifyReport.fail", testFmtVerifyFail),
      ("fmtVerify.multi", testFmtVerifyMulti),
      ("fmtCombinedVerify", testFmtCombinedVerify) ]
  do
    let code ← t
    if code != 0 then
      IO.eprintln s!"FAIL: {label}"
      anyFail := 1
  if anyFail == 0 then
    IO.println "format regression tests passed"
  return anyFail
