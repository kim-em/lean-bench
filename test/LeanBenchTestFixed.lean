import LeanBench

/-!
# Fixed-benchmark tests

End-to-end coverage for `setup_fixed_benchmark` and the fixed
parent / child path:

1. **Roundtrip**: a fixed-benchmark JSONL row emitted by the child
   parses back into a `FixedDataPoint` with the same fields.
2. **Pure path**: a registered pure value runs through the full
   spawn / parse pipeline; `repeats` measured calls produce that
   many `ok` data points with consistent hashes.
3. **IO path**: same, but for an `IO α` registration.
4. **Hash agreement across repeats**: a deterministic benchmark
   reports `hashesAgree := true`. This pins the cross-repeat hash
   check (a tripwire for non-deterministic registrations).
5. **Compare**: two registered fixed benchmarks comparing equal
   workloads produce a relative-timing report and report
   `allAgreed` on hashes.
6. **List and verify**: both registries appear in the unified
   `list` output; `verify` runs the fixed registry and reports
   passing checks.
7. **Kill-on-cap**: a deliberately-slow fixed benchmark with a tight
   `maxSecondsPerCall` produces a `killedAtCap` repeat — same
   machinery as the parametric kill-path test.
-/

namespace LeanBench.Test.Fixed

/-- A trivial fixed benchmark. The `Unit →` wrapper is required
    by `setup_fixed_benchmark` (issue #54). -/
def cheapPure : Unit → UInt64 := fun () => Id.run do
  let mut a : UInt64 := 0
  let mut b : UInt64 := 1
  for _ in [0:50] do
    let c := a + b
    a := b
    b := c
  return a

/-- Same workload, but exposed as an `IO` action. -/
def cheapIO : IO UInt64 := return cheapPure ()

/-- Same workload, but exposed as an `EIO IO.Error` action — the
    fully-spelled-out form that `IO α` reduces to. The macro must
    detect this as IO via semantic isDefEq, not raw head-syntax. -/
def cheapEIO : EIO IO.Error UInt64 := return cheapPure ()

/-- Same workload, but exposed via a reducible alias of `IO`. The
    macro must reduce the alias before checking the IO shape. -/
@[reducible] def MyIO (α : Type) : Type := IO α
def cheapMyIO : MyIO UInt64 := return cheapPure ()

/-- `Unit → IO α` shape — the third accepted callable form. -/
def cheapIOUnit : Unit → IO UInt64 := fun () => return cheapPure ()

/-- Deliberately-slow workload for the kill-on-cap test. Loops long
    enough to exceed the 0-cap reliably. -/
def slowPure : Unit → UInt64 := fun () => Id.run do
  let mut x : UInt64 := 0
  for _ in [0:1_000_000] do
    for _ in [0:1000] do
      x := x ^^^ 1
  return x

/-- Synonym for the correct-`expectedHash` test (issue #55); a second
    registration of `cheapPure` needs a distinct name. -/
def cheapPureCorrect : Unit → UInt64 := cheapPure

/-- Synonym for the wrong-`expectedHash` test (issue #55). -/
def cheapPureRegressed : Unit → UInt64 := cheapPure

setup_fixed_benchmark cheapPure   where { repeats := 3 }
setup_fixed_benchmark cheapIO     where { repeats := 3 }
setup_fixed_benchmark cheapEIO    where { repeats := 2 }
setup_fixed_benchmark cheapMyIO   where { repeats := 2 }
setup_fixed_benchmark cheapIOUnit where { repeats := 2 }
setup_fixed_benchmark slowPure    where { repeats := 1, maxSecondsPerCall := 5.0 }
setup_fixed_benchmark cheapPureCorrect where {
  repeats := 2
  expectedHash := some (Hashable.hash (cheapPure ())) }
setup_fixed_benchmark cheapPureRegressed where {
  repeats := 2
  -- Deliberately wrong: bit-flipped expected hash. The runner produces
  -- `Hashable.hash (cheapPure ())`, so the check must fail.
  expectedHash := some ((Hashable.hash (cheapPure ())) ^^^ 0xffffffffffffffff) }

end LeanBench.Test.Fixed

open LeanBench

private def cheapPureName          : Lean.Name := `LeanBench.Test.Fixed.cheapPure
private def cheapIOName            : Lean.Name := `LeanBench.Test.Fixed.cheapIO
private def cheapEIOName           : Lean.Name := `LeanBench.Test.Fixed.cheapEIO
private def cheapMyIOName          : Lean.Name := `LeanBench.Test.Fixed.cheapMyIO
private def cheapIOUnitName        : Lean.Name := `LeanBench.Test.Fixed.cheapIOUnit
private def slowPureName           : Lean.Name := `LeanBench.Test.Fixed.slowPure
private def cheapPureCorrectName   : Lean.Name :=
  `LeanBench.Test.Fixed.cheapPureCorrect
private def cheapPureRegressedName : Lean.Name :=
  `LeanBench.Test.Fixed.cheapPureRegressed

/-- Substring helper. -/
private def stringContains (haystack needle : String) : Bool :=
  ((haystack.splitOn needle).length) > 1

def testRoundtrip : IO UInt32 := do
  let row :=
    "{\"schema_version\":1,\"kind\":\"fixed\",\"function\":\"foo.bar\"," ++
    "\"repeat_index\":2,\"total_nanos\":1234567,\"result_hash\":\"0xcafebabe\"," ++
    "\"status\":\"ok\",\"error\":null}"
  match parseFixedChildRow row with
  | .error e =>
    IO.eprintln s!"parse failure: {e}"
    return 1
  | .ok dp =>
    let ok :=
      dp.repeatIndex == 2 ∧
      dp.totalNanos == 1234567 ∧
      dp.resultHash == some 0xcafebabe ∧
      dp.status == .ok
    if ok then return 0
    IO.eprintln s!"roundtrip mismatch: {repr dp}"
    return 1

def testPurePath : IO UInt32 := do
  let result ← runFixedBenchmark cheapPureName
  if result.points.size != 3 then
    IO.eprintln s!"expected 3 repeats, got {result.points.size}"
    return 1
  for dp in result.points do
    unless dp.status == .ok do
      IO.eprintln s!"unexpected non-ok status: {repr dp}"
      return 1
    unless dp.resultHash.isSome do
      IO.eprintln s!"hashable benchmark missing result_hash"
      return 1
  unless result.hashesAgree do
    IO.eprintln "expected deterministic benchmark to have hashesAgree = true"
    return 1
  unless result.medianNanos?.isSome do
    IO.eprintln "expected medianNanos? = some _"
    return 1
  return 0

def testIOPath : IO UInt32 := do
  let result ← runFixedBenchmark cheapIOName
  if result.points.size != 3 then
    IO.eprintln s!"expected 3 repeats, got {result.points.size}"
    return 1
  for dp in result.points do
    unless dp.status == .ok do
      IO.eprintln s!"IO path: unexpected status: {repr dp}"
      return 1
  unless result.hashesAgree do
    IO.eprintln "IO path: expected hashesAgree = true"
    return 1
  return 0

/-- The macro must detect `EIO IO.Error α` (the full form `IO`
    reduces to) and a reducible `MyIO α` alias as IO, not as a pure
    value. A failure here would silently benchmark the IO action
    object instead of executing it, which is a serious correctness
    bug. We assert that the registered runners produce `ok` repeats
    with `Hashable` results — both impossible if the macro mis-routed
    these as pure values. -/
def testIOAliasDetection : IO UInt32 := do
  for name in [cheapEIOName, cheapMyIOName, cheapIOUnitName] do
    let result ← runFixedBenchmark name
    unless result.points.all (·.status == .ok) do
      IO.eprintln s!"{name}: expected all ok, got {repr result.points}"
      return 1
    unless result.hashesAgree do
      IO.eprintln s!"{name}: expected hashesAgree = true"
      return 1
    -- The runner must have actually executed the IO action and
    -- produced the same hash as the pure benchmark — otherwise it's
    -- benchmarking the action thunk, not the computation.
    let pureResult ← runFixedBenchmark cheapPureName
    let pureHash := pureResult.points.findSome? fun dp =>
      match dp.status, dp.resultHash with
      | .ok, some h => some h
      | _,   _      => none
    let ioHash := result.points.findSome? fun dp =>
      match dp.status, dp.resultHash with
      | .ok, some h => some h
      | _,   _      => none
    unless pureHash == ioHash do
      IO.eprintln s!"{name}: hash {ioHash} differs from pure {pureHash} — likely benchmarked the IO thunk, not the computation"
      return 1
  return 0

def testCompare : IO UInt32 := do
  let report ← compareFixed [cheapPureName, cheapIOName]
  if report.results.size != 2 then
    IO.eprintln s!"expected 2 results, got {report.results.size}"
    return 1
  match report.agreeOnHash with
  | .allAgreed => pure ()
  | .hashUnavailable unhashed =>
    IO.eprintln s!"expected allAgreed (both benchmarks are Hashable); unhashed={unhashed.toList}"
    return 1
  | .diverged detail =>
    IO.eprintln s!"expected allAgreed, got divergence: {repr detail}"
    return 1
  -- The formatter must surface the relative-timing line.
  let rendered := Format.fmtFixedComparison report
  unless stringContains rendered "relative median" do
    IO.eprintln s!"expected relative-median line in output, got:\n{rendered}"
    return 1
  return 0

def testListAndVerify : IO UInt32 := do
  -- The fixed registry must contain our three test benchmarks.
  let entries ← allFixedRuntimeEntries
  unless entries.size ≥ 3 do
    IO.eprintln s!"expected ≥ 3 fixed entries, got {entries.size}"
    return 1
  -- `verify` should pass for the cheap benchmarks (slowPure has a
  -- 5s cap, also cheap enough to pass at this small loop size).
  let reports ← verify [cheapPureName, cheapIOName]
  unless reports.parametric.isEmpty do
    IO.eprintln s!"expected parametric to be empty, got {reports.parametric.size}"
    return 1
  unless reports.fixed.size == 2 do
    IO.eprintln s!"expected 2 fixed verify reports, got {reports.fixed.size}"
    return 1
  unless reports.passed do
    IO.eprintln s!"expected all fixed verify reports to pass:\n{Format.fmtCombinedVerify reports}"
    return 1
  return 0

def testKillOnCap : IO UInt32 := do
  let some entry ← findFixedRuntimeEntry slowPureName
    | do
        IO.eprintln "kill-cap test: registry lookup failed"
        return 1
  let tightCfg : FixedBenchmarkConfig :=
    { entry.spec.config with
      maxSecondsPerCall := 0
      killGraceMs := 100 }
  let dp ← runFixedOneBatch entry.spec tightCfg 0
  match dp.status with
  | .killedAtCap => return 0
  | s =>
    IO.eprintln s!"kill-cap FAIL: expected killedAtCap, got {repr s}"
    return 1

/-- Observed hash is populated and rendered even with no
    `expectedHash` declared. Issue #55. -/
def testObservedHashSurfaced : IO UInt32 := do
  let result ← runFixedBenchmark cheapPureName
  -- No expected hash is declared on the bare `cheapPure` registration.
  unless result.config.expectedHash.isNone do
    IO.eprintln "expected default expectedHash to be none"
    return 1
  -- But the observed hash must be populated and match the canonical
  -- `Hashable.hash cheapPure` value (the runner is `let r := fnId; let
  -- h := Hashable.hash r`).
  unless result.observedHash? == some (Hashable.hash (LeanBench.Test.Fixed.cheapPure ())) do
    IO.eprintln s!"observedHash? mismatch: got {result.observedHash?}"
    return 1
  -- The expected-hash check should be `.unset` when nothing was
  -- declared.
  unless result.expectedHashCheck == .unset do
    IO.eprintln s!"expected .unset, got {repr result.expectedHashCheck}"
    return 1
  -- The formatter must surface the observed hash so authors can read
  -- it off the run output.
  let rendered := Format.fmtFixedResult result
  unless stringContains rendered "observed hash:" do
    IO.eprintln s!"expected 'observed hash:' line in render, got:\n{rendered}"
    return 1
  return 0

/-- Correct `expectedHash` → `.match`. Issue #55. -/
def testExpectedHashMatch : IO UInt32 := do
  let result ← runFixedBenchmark cheapPureCorrectName
  unless result.expectedHashCheck == .match do
    IO.eprintln s!"expected .match, got {repr result.expectedHashCheck}"
    return 1
  unless result.expectedHashCheck.passed do
    IO.eprintln "expected .match to be passed"
    return 1
  let rendered := Format.fmtFixedResult result
  unless stringContains rendered "expected hash: matches" do
    IO.eprintln s!"expected match line in render, got:\n{rendered}"
    return 1
  return 0

/-- Wrong `expectedHash` → `.mismatch` with `passed := false`; the
    formatter renders a FAIL line. Issue #55. -/
def testExpectedHashMismatch : IO UInt32 := do
  let result ← runFixedBenchmark cheapPureRegressedName
  let expected := (Hashable.hash (LeanBench.Test.Fixed.cheapPure ())) ^^^ 0xffffffffffffffff
  let got := Hashable.hash (LeanBench.Test.Fixed.cheapPure ())
  unless result.expectedHashCheck == .mismatch expected got do
    IO.eprintln s!"expected .mismatch {expected} {got}, got {repr result.expectedHashCheck}"
    return 1
  if result.expectedHashCheck.passed then
    IO.eprintln "expected .mismatch to NOT be passed"
    return 1
  let rendered := Format.fmtFixedResult result
  unless stringContains rendered "expected hash: FAIL" do
    IO.eprintln s!"expected FAIL line in render, got:\n{rendered}"
    return 1
  return 0

/-- `expectedHash` and `observedHash?` survive JSON round-trip; the
    derived `expectedHashCheck` reconstructs identically. Issue #55. -/
def testExpectedHashRoundtrip : IO UInt32 := do
  let result ← runFixedBenchmark cheapPureCorrectName
  let json := Export.fixedResultToJson result
  match Export.fixedResultFromJson json with
  | .error e =>
    IO.eprintln s!"roundtrip parse error: {e}"
    return 1
  | .ok r' =>
    unless r'.config.expectedHash == result.config.expectedHash do
      IO.eprintln s!"expectedHash roundtrip mismatch: {r'.config.expectedHash} vs {result.config.expectedHash}"
      return 1
    unless r'.observedHash? == result.observedHash? do
      IO.eprintln s!"observedHash? roundtrip mismatch: {r'.observedHash?} vs {result.observedHash?}"
      return 1
    unless r'.expectedHashCheck == result.expectedHashCheck do
      IO.eprintln s!"expectedHashCheck roundtrip mismatch: {repr r'.expectedHashCheck} vs {repr result.expectedHashCheck}"
      return 1
    return 0

/-- Pure projection unit tests for `FixedResult.expectedHashCheck`:
    pin the edge cases (non-Hashable, hashesAgree=false,
    noObservedHash) without engineering rare runtime conditions.
    Issue #55. -/
def testExpectedHashCheckProjection : IO UInt32 := do
  let baseCfg : FixedBenchmarkConfig := { expectedHash := some 0xabcd }
  let mkResult (hashable : Bool) (hashesAgree : Bool)
      (observedHash? : Option UInt64) : FixedResult :=
    { function := `dummy
      hashable
      config := baseCfg
      points := #[]
      medianNanos? := none
      minNanos?    := none
      maxNanos?    := none
      hashesAgree
      observedHash? }
  -- Non-Hashable benchmark with expectedHash declared: the check
  -- collapses to .unset (no hash to compare against; matches docs).
  let r1 := mkResult false true none
  unless r1.expectedHashCheck == .unset do
    IO.eprintln s!"non-hashable: expected .unset, got {repr r1.expectedHashCheck}"
    return 1
  -- Hashable, hashesAgree=false: any single observation is unsafe;
  -- expectedHashCheck must fail with .inconsistentAcrossRepeats even
  -- if observedHash? happens to equal expected.
  let r2 := mkResult true false (some 0xabcd)
  unless r2.expectedHashCheck == .inconsistentAcrossRepeats 0xabcd do
    IO.eprintln s!"disagree+match: expected .inconsistentAcrossRepeats, got {repr r2.expectedHashCheck}"
    return 1
  if r2.expectedHashCheck.passed then
    IO.eprintln "disagree+match: expected NOT passed"
    return 1
  -- Hashable, hashesAgree=true, no observed hash (every repeat
  -- failed before producing one): .noObservedHash, failure.
  let r3 := mkResult true true none
  unless r3.expectedHashCheck == .noObservedHash 0xabcd do
    IO.eprintln s!"no observed: expected .noObservedHash, got {repr r3.expectedHashCheck}"
    return 1
  -- Sanity: with no expectedHash declared, the check is .unset
  -- regardless of observed state.
  let baseCfgUnset : FixedBenchmarkConfig := {}
  let r4 : FixedResult :=
    { function := `dummy, hashable := true, config := baseCfgUnset, points := #[]
      medianNanos? := none, minNanos? := none, maxNanos? := none
      hashesAgree := true, observedHash? := some 0xdeadbeef }
  unless r4.expectedHashCheck == .unset do
    IO.eprintln s!"unset config: expected .unset, got {repr r4.expectedHashCheck}"
    return 1
  return 0

/-- `--ignore-expected-hash` (CLI escape hatch) clears `expectedHash`
    via the override; a known-wrong expected then no longer fails.
    Issue #55. -/
def testIgnoreExpectedHashOverride : IO UInt32 := do
  let result ← runFixedBenchmark cheapPureRegressedName
    (override := { ignoreExpectedHash := true })
  unless result.config.expectedHash.isNone do
    IO.eprintln "expected override to clear expectedHash"
    return 1
  unless result.expectedHashCheck == .unset do
    IO.eprintln s!"expected .unset under override, got {repr result.expectedHashCheck}"
    return 1
  return 0

/-- Wrong `expectedHash` fails `verify` too, not just `run`. Issue #55. -/
def testExpectedHashVerify : IO UInt32 := do
  let reports ← verify [cheapPureRegressedName]
  unless reports.fixed.size == 1 do
    IO.eprintln s!"expected 1 fixed verify report, got {reports.fixed.size}"
    return 1
  let rep := reports.fixed[0]!
  if rep.passed then
    IO.eprintln "verify expected to fail on wrong expectedHash, but passed"
    return 1
  let rendered := Format.fmtFixedVerifyReport rep
  unless stringContains rendered "expectedHash mismatch" do
    IO.eprintln s!"expected 'expectedHash mismatch' in verify render, got:\n{rendered}"
    return 1
  return 0

def testFormat : IO UInt32 := do
  -- The list formatter must annotate fixed entries.
  let entries ← allFixedRuntimeEntries
  let some entry := entries[0]?
    | do
        IO.eprintln "format test: no fixed entries to format"
        return 1
  let line := Format.fmtFixedSpec entry.spec
  unless stringContains line "[fixed]" do
    IO.eprintln s!"expected '[fixed]' annotation in: {line}"
    return 1
  return 0

/-- `medianNat` reports the upper of the two middle elements for
    even-length arrays — the conservative choice for regression
    detection. Pin that contract so a future "lower-middle" change
    is intentional, not accidental. With `repeats := 2`, the
    `medianNanos?` should equal the slower of the two `ok` repeats. -/
def testEvenRepeatsMedian : IO UInt32 := do
  -- cheapEIO is registered with `repeats := 2`.
  let result ← runFixedBenchmark cheapEIOName
  if result.points.size != 2 then
    IO.eprintln s!"expected 2 repeats, got {result.points.size}"
    return 1
  let okNanos : Array Nat := result.points.filterMap fun dp =>
    match dp.status with
    | .ok => some dp.totalNanos
    | _   => none
  if okNanos.size != 2 then
    IO.eprintln s!"expected 2 ok repeats; got nanos {okNanos}"
    return 1
  let upper := if okNanos[0]! > okNanos[1]! then okNanos[0]! else okNanos[1]!
  match result.medianNanos? with
  | some m =>
    unless m == upper do
      IO.eprintln s!"expected upper-median {upper}, got {m} (raw nanos {okNanos})"
      return 1
    return 0
  | none =>
    IO.eprintln "expected medianNanos? to be some _"
    return 1

def runTests : IO UInt32 := do
  for (label, t) in
    [("roundtrip", testRoundtrip),
     ("purePath", testPurePath),
     ("ioPath", testIOPath),
     ("ioAliasDetection", testIOAliasDetection),
     ("evenRepeatsMedian", testEvenRepeatsMedian),
     ("compare", testCompare),
     ("listAndVerify", testListAndVerify),
     ("killOnCap", testKillOnCap),
     ("format", testFormat),
     ("observedHashSurfaced", testObservedHashSurfaced),
     ("expectedHashMatch", testExpectedHashMatch),
     ("expectedHashMismatch", testExpectedHashMismatch),
     ("expectedHashRoundtrip", testExpectedHashRoundtrip),
     ("expectedHashCheckProjection", testExpectedHashCheckProjection),
     ("ignoreExpectedHashOverride", testIgnoreExpectedHashOverride),
     ("expectedHashVerify", testExpectedHashVerify)]
  do
    let code ← t
    if code != 0 then
      IO.eprintln s!"FAIL: {label}"
      return code
    IO.println s!"  ok  {label}"
  IO.println "fixed benchmark tests passed"
  return 0

/-- Same compiled binary acts as parent (test driver) and child
    (the `_child` first arg distinguishes them). -/
def main (args : List String) : IO UInt32 :=
  match args with
  | "_child" :: _ => LeanBench.Cli.dispatch args
  | "_probe_floor" :: _ => LeanBench.Cli.dispatch args
  | _ => runTests
