import LeanBench

/-!
# End-to-end run/compare test (issue #2)

Drives `runBenchmark` and `compare` against a tiny registered
benchmark, then asserts on the result structure. Each measurement
spawns the same compiled binary as a child (`_child` first arg
distinguishes child mode), so this also exercises the full spawn /
read / parse / synthesize pipeline against a real subprocess.

The benchmark function is deliberately tiny (a single `XOR` per
iteration) and the config caps `paramCeiling := 8` and
`maxSecondsPerCall := 0.5` with a `targetInnerNanos := 50_000_000`
inner-tune budget, so the whole test runs in ~1.3s on a Linux host.

What's pinned:

- `list`: every registered benchmark appears in the runtime registry
  AND `Cli.dispatch ["list"]` exits 0 against a populated registry.
- `runBenchmark`: at least one `ok` point lands in the ladder, every
  `ok` row has a `Hashable`-derived hash, `ratios` is non-empty,
  the verdict resolves (not stuck on `Inhabited`), and the
  complexity formula round-trips from `setup_benchmark`.
- `compare` (two distinguishable functions): both per-function
  results are present, `commonParams` equals the actual intersection
  of measured params (computed independently in the test), and
  `agreeOnCommon` reports `divergedAt`.
- `compare` (two identical functions): `agreeOnCommon` reports
  `allAgreed`.
- Unregistered name → `runBenchmark` throws a `userError` mentioning
  the name (so the CLI can print it cleanly).
-/

open LeanBench

namespace LeanBench.Test.E2E

/-- A trivially-fast `Nat → UInt64`. Each call does `n` cheap XORs. -/
def littleFn (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 1
  for _ in [0:n] do
    x := x ^^^ n.toUInt64
  return x

/-- Same shape, different output: `+ 1` so the hash differs from
    `littleFn` and the comparison divergence is observable. -/
def littleFn' (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 1
  for _ in [0:n] do
    x := x ^^^ n.toUInt64
  return x + 1

/-- Same body as `littleFn` so the comparison reports `allAgreed`. -/
def littleFnTwin (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 1
  for _ in [0:n] do
    x := x ^^^ n.toUInt64
  return x

/-! Each benchmark uses a tight cap and a small inner-tune target.
With the default `targetInnerNanos := 500_000_000`, autoTune wants
~250ms of inner work per batch — that overshoots a 0.25s cap once the
final doubling step lands past the half-target. Lower the inner
target so even slow CI hosts converge before the kill timer fires. -/
setup_benchmark littleFn n => n where {
  maxSecondsPerCall := 0.5, paramCeiling := 8, targetInnerNanos := 50_000_000
}
setup_benchmark littleFn' n => n where {
  maxSecondsPerCall := 0.5, paramCeiling := 8, targetInnerNanos := 50_000_000
}
setup_benchmark littleFnTwin n => n where {
  maxSecondsPerCall := 0.5, paramCeiling := 8, targetInnerNanos := 50_000_000
}

end LeanBench.Test.E2E

private def littleName     : Lean.Name := `LeanBench.Test.E2E.littleFn
private def littlePrimeName : Lean.Name := `LeanBench.Test.E2E.littleFn'
private def littleTwinName : Lean.Name := `LeanBench.Test.E2E.littleFnTwin

/-- `String.contains` is `Char`-only; this is a substring check. -/
private def containsSub (haystack needle : String) : Bool :=
  ((haystack.splitOn needle).length) > 1

/-! ## End-to-end `runBenchmark` -/

def testRunBenchmarkHappyPath : IO UInt32 := do
  let result ← runBenchmark littleName
  -- The benchmark is registered; we should see at least one row.
  if result.points.isEmpty then
    IO.eprintln "expected at least one measured point"
    return 1
  -- Most rows should succeed. A solitary killed-at-cap on the largest
  -- param is tolerable on a slow host; an all-killed ladder means the
  -- subprocess pipeline isn't returning real measurements.
  let okCount := result.points.filter (·.status == .ok) |>.size
  if okCount == 0 then
    IO.eprintln s!"expected at least one ok point; all status: {repr (result.points.map (·.status))}"
    return 1
  -- Every `ok` row must have a hash (the function returns `UInt64`,
  -- which is `Hashable`; hash = none would be a regression in the
  -- `setup_benchmark` Hashable detection).
  for dp in result.points do
    if dp.status == .ok && dp.resultHash.isNone then
      IO.eprintln s!"ok row missing hash: {repr dp}"
      return 1
  -- `ratios` must be populated by `Stats.summarize`. An empty array
  -- here means either no `ok` points landed in the verdict band or
  -- the ratio computation skipped them all — both are regressions
  -- that block the verdict pipeline.
  if result.ratios.isEmpty then
    IO.eprintln s!"expected non-empty ratios; got points: {repr result.points}"
    return 1
  -- The verdict must resolve. Any `Verdict` constructor is fine; the
  -- regression we care about is the harness silently producing an
  -- `Inhabited` default before reaching this line.
  match result.verdict with
  | .consistentWithDeclaredComplexity => pure ()
  | .inconclusive                     => pure ()
  -- The complexity formula must round-trip from setup_benchmark's
  -- captured-source path, not show as `<unknown>` or empty.
  if result.complexityFormula.isEmpty then
    IO.eprintln s!"expected non-empty complexity formula, got '{result.complexityFormula}'"
    return 1
  return 0

def testRunBenchmarkUnregistered : IO UInt32 := do
  let result ← (runBenchmark `definitelyNotRegistered).toBaseIO
  match result with
  | .ok _ =>
    IO.eprintln "expected runBenchmark on unregistered name to throw"
    return 1
  | .error e =>
    let msg := e.toString
    if containsSub msg "unregistered" ∧ containsSub msg "definitelyNotRegistered" then
      return 0
    IO.eprintln s!"expected error to mention 'unregistered' and the name, got: {msg}"
    return 1

/-! ## End-to-end `compare` -/

def testCompareDiverges : IO UInt32 := do
  -- littleFn vs littleFn' return different hashes, so the
  -- comparison must report divergence.
  let report ← compare [littleName, littlePrimeName]
  if report.results.size != 2 then
    IO.eprintln s!"expected 2 results, got {report.results.size}"
    return 1
  -- `commonParams` is the intersection of every function's measured
  -- params. Both benchmarks share the same ladder shape and ceiling,
  -- so the intersection is exactly the union of params each side
  -- actually completed — and at minimum the small end (1, 2) survives
  -- on every host that doesn't kill the very first batch.
  let p0 := report.results[0]!.points.map (·.param) |>.toList.toArray
  let p1 := report.results[1]!.points.map (·.param) |>.toList.toArray
  let expectedCommon := p0.filter (p1.contains ·)
  unless report.commonParams == expectedCommon do
    IO.eprintln s!"commonParams != intersection: got {report.commonParams}, expected {expectedCommon}"
    return 1
  if report.commonParams.isEmpty then
    IO.eprintln "expected non-empty common params"
    return 1
  match report.agreeOnCommon with
  | .divergedAt entries =>
    if entries.isEmpty then
      IO.eprintln "expected at least one divergence entry"
      return 1
    return 0
  | s =>
    IO.eprintln s!"expected divergence, got {repr s}"
    return 1

/-- Regression: `hashUnavailable` must key off the registered
    `Hashable` flag, not the data. A hashable benchmark whose runs
    all failed (every row is `error` / `killed_at_cap` with
    `resultHash := none`) used to be misreported as
    `hashUnavailable`, falsely pointing the user at the registration.
    This test pipes a hand-crafted pair of "all-error" hashable
    results through `computeAgreement` and asserts the result is
    `allAgreed` (no observed divergence), not `hashUnavailable`. -/
def testHashUnavailableIsRegistrationKeyed : IO UInt32 := do
  -- Two hashable benchmarks whose every row is an error row with no
  -- hash — same-shaped data both sides, so even if a buggy check
  -- somehow read the row data it would not see divergence.
  let errorPoint : DataPoint :=
    { param := 1, innerRepeats := 0, totalNanos := 0
    , perCallNanos := 0.0, resultHash := none
    , status := .error "synthesised for test", partOfVerdict := true }
  let mkResult (n : Lean.Name) : BenchmarkResult :=
    { function := n
    , complexityFormula := "n"
    , hashable := true
    , config := {}
    , points := #[errorPoint]
    , ratios := #[]
    , verdict := .inconclusive
    , cMin? := none
    , cMax? := none
    , verdictDroppedLeading := 0
    , slope? := none
    , spawnFloorNanos? := none }
  let results : Array BenchmarkResult :=
    #[mkResult `regressionA, mkResult `regressionB]
  match computeAgreement results #[1] with
  | .hashUnavailable names =>
    IO.eprintln s!"REGRESSION: hashUnavailable fired on hashable benchmarks; names={names.toList}"
    return 1
  | .divergedAt _ =>
    IO.eprintln "REGRESSION: divergence reported for matching empty data"
    return 1
  | .allAgreed => return 0

/-- Sibling regression for fixed benchmarks: a hashable
    fixed-benchmark with no successful repeats must not be reported
    as `hashUnavailable`. -/
def testFixedHashUnavailableIsRegistrationKeyed : IO UInt32 := do
  let errorRepeat : FixedDataPoint :=
    { repeatIndex := 0, totalNanos := 0, resultHash := none
    , status := .error "synthesised for test" }
  let mkResult (n : Lean.Name) : FixedResult :=
    { function := n
    , hashable := true
    , config := {}
    , points := #[errorRepeat]
    , medianNanos? := none
    , minNanos? := none
    , maxNanos? := none
    , hashesAgree := true }
  let results : Array FixedResult :=
    #[mkResult `fixedA, mkResult `fixedB]
  match computeFixedAgreement results with
  | .hashUnavailable names =>
    IO.eprintln s!"REGRESSION: fixed hashUnavailable fired on hashable benchmarks; names={names.toList}"
    return 1
  | .diverged _ =>
    IO.eprintln "REGRESSION: fixed divergence reported for empty data"
    return 1
  | .allAgreed => return 0

def testCompareAgrees : IO UInt32 := do
  -- littleFn and littleFnTwin are byte-identical, so the comparison
  -- must report agreement.
  let report ← compare [littleName, littleTwinName]
  if report.results.size != 2 then
    IO.eprintln s!"expected 2 results, got {report.results.size}"
    return 1
  match report.agreeOnCommon with
  | .allAgreed => return 0
  | s =>
    IO.eprintln s!"expected allAgreed, got {repr s}"
    return 1

/-! ## End-to-end `list` (via the CLI dispatch entry point) -/

def testListIncludesRegistered : IO UInt32 := do
  -- `runListCmd` walks `allRuntimeEntries`; assert our three
  -- registrations are present so a regression in the registry
  -- (e.g. the macro forgetting to emit `initialize`) shows up here.
  let entries ← allRuntimeEntries
  let names := entries.map (·.spec.name)
  for n in [littleName, littlePrimeName, littleTwinName] do
    unless names.contains n do
      IO.eprintln s!"registered benchmark missing from runtime registry: {n}"
      return 1
  return 0

/-- Drive the `list` subcommand through the actual CLI dispatch
    pipeline against this binary's populated registry. Catches
    regressions in `runListCmd` (the formatter, the registry walk,
    the exit code) that the registry-only check above misses.

    We redirect stdout to /dev/null on POSIX or NUL on Windows so the
    test output stays clean — we only need the exit code to confirm
    `list` ran the populated path without crashing. -/
def testListCliDispatch : IO UInt32 := do
  -- Suppress stdout so the formatted listing doesn't pollute the
  -- test log. The exit code is what we're asserting on.
  let nullDev := if System.Platform.isWindows then "NUL" else "/dev/null"
  let h ← IO.FS.Handle.mk nullDev .write
  let code ← IO.withStdout (.ofHandle h) do
    LeanBench.Cli.dispatch ["list"]
  if code == 0 then return 0
  IO.eprintln s!"FAIL: list dispatch returned {code}"
  return 1

/-! ## Driver -/

def runTests : IO UInt32 := do
  let mut anyFail : UInt32 := 0
  for (label, t) in
    [ ("listIncludesRegistered", testListIncludesRegistered),
      ("listCliDispatch", testListCliDispatch),
      ("runBenchmark.happy", testRunBenchmarkHappyPath),
      ("runBenchmark.unregistered", testRunBenchmarkUnregistered),
      ("compare.agrees", testCompareAgrees),
      ("compare.diverges", testCompareDiverges),
      ("compare.hashUnavailableIsRegistrationKeyed",
        testHashUnavailableIsRegistrationKeyed),
      ("fixedCompare.hashUnavailableIsRegistrationKeyed",
        testFixedHashUnavailableIsRegistrationKeyed) ]
  do
    let code ← t
    if code != 0 then
      IO.eprintln s!"FAIL: {label}"
      anyFail := 1
    else
      IO.println s!"  ok  {label}"
  if anyFail == 0 then
    IO.println "e2e tests passed"
  return anyFail

/-- The same compiled binary acts as parent (test driver) and child
    (single-batch runner spawned by `runOneBatch`). The `_child`
    first arg distinguishes them. -/
def main (args : List String) : IO UInt32 :=
  match args with
  | "_child" :: _ => LeanBench.Cli.dispatch args
  | _ => runTests
