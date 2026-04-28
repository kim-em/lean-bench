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

/-- A trivial pure fixed benchmark. Uses a small computation so
    test runtime is short. -/
def cheapPure : UInt64 := Id.run do
  let mut a : UInt64 := 0
  let mut b : UInt64 := 1
  for _ in [0:50] do
    let c := a + b
    a := b
    b := c
  return a

/-- Same workload, but exposed as an `IO` action. -/
def cheapIO : IO UInt64 := return cheapPure

/-- Same workload, but exposed as an `EIO IO.Error` action — the
    fully-spelled-out form that `IO α` reduces to. The macro must
    detect this as IO via semantic isDefEq, not raw head-syntax. -/
def cheapEIO : EIO IO.Error UInt64 := return cheapPure

/-- Same workload, but exposed via a reducible alias of `IO`. The
    macro must reduce the alias before checking the IO shape. -/
@[reducible] def MyIO (α : Type) : Type := IO α
def cheapMyIO : MyIO UInt64 := return cheapPure

/-- Deliberately-slow workload for the kill-on-cap test. Loops long
    enough to exceed the 0-cap reliably. -/
def slowPure : UInt64 := Id.run do
  let mut x : UInt64 := 0
  for _ in [0:1_000_000] do
    for _ in [0:1000] do
      x := x ^^^ 1
  return x

setup_fixed_benchmark cheapPure where { repeats := 3 }
setup_fixed_benchmark cheapIO   where { repeats := 3 }
setup_fixed_benchmark cheapEIO  where { repeats := 2 }
setup_fixed_benchmark cheapMyIO where { repeats := 2 }
setup_fixed_benchmark slowPure  where { repeats := 1, maxSecondsPerCall := 5.0 }

end LeanBench.Test.Fixed

open LeanBench

private def cheapPureName : Lean.Name := `LeanBench.Test.Fixed.cheapPure
private def cheapIOName   : Lean.Name := `LeanBench.Test.Fixed.cheapIO
private def cheapEIOName  : Lean.Name := `LeanBench.Test.Fixed.cheapEIO
private def cheapMyIOName : Lean.Name := `LeanBench.Test.Fixed.cheapMyIO
private def slowPureName  : Lean.Name := `LeanBench.Test.Fixed.slowPure

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
  for name in [cheapEIOName, cheapMyIOName] do
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
  | .hashUnavailable =>
    IO.eprintln "expected allAgreed (both benchmarks are Hashable)"
    return 1
  | .diverged entries =>
    IO.eprintln s!"expected allAgreed, got divergence: {repr entries}"
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
     ("format", testFormat)]
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
  | _ => runTests
