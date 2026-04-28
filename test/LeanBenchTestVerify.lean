import LeanBench

/-!
# `verify` command tests

Coverage:

1. `verify` against a working benchmark passes; both `f 0` and
   `f 1` are exercised, and since the return type is `Hashable` a
   hash is produced for every check.
2. `verify` against a non-existent name throws a `userError` so the
   CLI can report it cleanly.
3. End-to-end subprocess failure: `verifyOne` against a spec whose
   name is not registered in the runtime registry produces a failed
   report. This exercises the real spawn → child error path → parent
   classification pipeline, not just the synthetic `VerifyReport`
   formatting path.
4. `classifyCheck` distinguishes pass from fail in all four
   meaningful (status × hashable × hash-present) combinations.
5. The formatter marks a failing report, lists every failure, and
   the multi-report summary line counts failures correctly.
-/

namespace LeanBench.Test.Verify

/-- A trivial, fast, hashable benchmark. -/
def trivialFn (n : Nat) : UInt64 := n.toUInt64

setup_benchmark trivialFn n => n + 1

end LeanBench.Test.Verify

open LeanBench

/-- Substring containment helper (`String.contains` is `Char`-based). -/
private def stringContains (haystack needle : String) : Bool :=
  ((haystack.splitOn needle).length) > 1

/-- The `setup_benchmark` macro registers names as proper hierarchical
    `Name`s (via `quote fnName`), so the test looks up the same shape. -/
private def benchmarkName : Lean.Name :=
  `LeanBench.Test.Verify.trivialFn

/-- Build a `DataPoint` shaped like the kind a child would emit. -/
private def mkOkPoint (param : Nat) (hash : Option UInt64 := some 0) : DataPoint :=
  { param, innerRepeats := 1, totalNanos := 100, perCallNanos := 100.0
  , resultHash := hash, status := .ok }

private def mkErrorPoint (param : Nat) (msg : String) : DataPoint :=
  { param, innerRepeats := 0, totalNanos := 0, perCallNanos := 0.0
  , resultHash := none, status := .error msg }

private def hashableSpec : BenchmarkSpec :=
  { name := `dummyHashable
  , complexityName := `dummy_complexity
  , complexityFormula := "<dummy>"
  , runCheckedName := `dummy_run
  , hashable := true
  , config := {} }

private def nonHashableSpec : BenchmarkSpec :=
  { hashableSpec with name := `dummyNoHash, hashable := false }

/-- `classifyCheck` invariants. -/
def testClassify : IO UInt32 := do
  let okHash      := classifyCheck hashableSpec    0 (mkOkPoint 0 (some 0xdeadbeef))
  let okNoHashH   := classifyCheck hashableSpec    0 (mkOkPoint 0 none)
  let okNoHashNH  := classifyCheck nonHashableSpec 0 (mkOkPoint 0 none)
  let err         := classifyCheck hashableSpec    0 (mkErrorPoint 0 "boom")
  if okHash.isSome then
    IO.eprintln "expected ok+hash to pass for hashable benchmark"
    return 1
  if okNoHashH.isNone then
    IO.eprintln "expected ok-without-hash to fail for hashable benchmark"
    return 1
  if okNoHashNH.isSome then
    IO.eprintln "expected ok-without-hash to pass for non-hashable benchmark"
    return 1
  if err.isNone then
    IO.eprintln "expected error status to fail"
    return 1
  return 0

/-- Verify of the registered `trivialFn` benchmark must pass.
    Pin the contract: both `f 0` and `f 1` are exercised. -/
def testHappyPath : IO UInt32 := do
  let reports ← LeanBench.verify [benchmarkName]
  if reports.size != 1 then
    IO.eprintln s!"expected 1 report, got {reports.size}"
    return 1
  let r := reports[0]!
  unless r.passed do
    IO.eprintln s!"verify report unexpectedly failed: {Format.fmtVerifyReport r}"
    return 1
  let exercisedParams := r.checks.map (·.param)
  unless exercisedParams == #[0, 1] do
    IO.eprintln s!"expected params #[0, 1], got {exercisedParams}"
    return 1
  for c in r.checks do
    unless c.point.resultHash.isSome do
      IO.eprintln s!"hashable benchmark missing result_hash at param={c.param}"
      return 1
  return 0

/-- End-to-end subprocess failure: a spec whose `name` is not in the
    runtime registry triggers the child's "unregistered benchmark"
    error path. The spawned child emits an error row and exits 1; the
    parent classifies the resulting `DataPoint` as a failure. This
    exercises the full pipeline (spawn, parse/synth, classify) for a
    deterministic failure without needing a hanging benchmark. -/
def testEndToEndFailure : IO UInt32 := do
  let bogusSpec : BenchmarkSpec :=
    { name := Lean.Name.mkSimple "definitely.not.registered"
    , complexityName := `dummy_complexity
    , complexityFormula := "<dummy>"
    , runCheckedName := `dummy_run
    , hashable := true
    , config := {} }
  let report ← verifyOne bogusSpec
  if report.passed then
    IO.eprintln s!"expected verifyOne on a fake spec to fail, got: {Format.fmtVerifyReport report}"
    return 1
  -- Every check should be flagged.
  unless report.checks.all (! ·.passed) do
    IO.eprintln s!"expected all checks to fail, got: {Format.fmtVerifyReport report}"
    return 1
  -- And the surfaced failure reason should mention the child failure,
  -- not (e.g.) a hash mismatch.
  let allMessages := report.checks.toList.filterMap (·.failure)
  unless allMessages.any (fun msg => stringContains msg "child") do
    IO.eprintln s!"expected failure messages to mention 'child', got: {allMessages}"
    return 1
  return 0

/-- Verify against a name that isn't registered must throw. -/
def testUnregistered : IO UInt32 := do
  let result ← (LeanBench.verify [`definitelyNotRegistered]).toBaseIO
  match result with
  | .error _ => return 0
  | .ok _ =>
    IO.eprintln "expected verify to throw on unregistered benchmark"
    return 1

/-- The formatter must mark a failing report and explain why. -/
def testFormatterFailure : IO UInt32 := do
  let failingReport : VerifyReport :=
    { spec := hashableSpec
    , checks := #[
        { param := 0
        , point := mkErrorPoint 0 "child exited with code 1"
        , failure := some "child error at param=0: child exited with code 1" } ] }
  let line := Format.fmtVerifyReport failingReport
  unless stringContains line "FAIL" do
    IO.eprintln s!"expected FAIL marker, got: {line}"
    return 1
  unless stringContains line "child error" do
    IO.eprintln s!"expected failure message, got: {line}"
    return 1
  let summary := Format.fmtVerify #[failingReport]
  unless stringContains summary "1 of 1" do
    IO.eprintln s!"expected '1 of 1' summary, got: {summary}"
    return 1
  return 0

/-- Run all verify tests, fail-fast. -/
def runTests : IO UInt32 := do
  for (label, t) in
    [("classify", testClassify),
     ("happyPath", testHappyPath),
     ("unregistered", testUnregistered),
     ("endToEndFailure", testEndToEndFailure),
     ("formatterFailure", testFormatterFailure)]
  do
    let code ← t
    if code != 0 then
      IO.eprintln s!"FAIL: {label}"
      return code
    IO.println s!"  ok  {label}"
  IO.println "verify tests passed"
  return 0

/-- The same compiled binary acts as parent (test driver) and child
    (single-batch runner spawned by `verify`). The presence of the
    `_child` first arg distinguishes them. -/
def main (args : List String) : IO UInt32 :=
  match args with
  | "_child" :: _ => LeanBench.Cli.dispatch args
  | _ => runTests
