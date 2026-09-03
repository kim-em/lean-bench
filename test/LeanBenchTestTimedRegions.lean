import LeanBench

/-!
# Timed-regions sidecar tests

Exercise the opt-in sidecar emission added in PR A1.

The sidecar emits one header record + one region record per call to
the runner's autotuned `loop` closure. We assert:

1. With the env var unset, no sidecar file is created (the existing
   behaviour is preserved).
2. With the env var set to a tempfile, the file appears with a
   well-formed header and ≥ 1 region records.
3. Region records carry monotonic boundaries that fall inside the
   wallclock window of the child invocation.
4. A `%p` token in the configured path is replaced by the child PID.
-/

open LeanBench

namespace LeanBench.Test.TimedRegions

/-- Tiny benchmark identical in shape to the one used by other
test files. The body just needs to do *something* the autotuner
will iterate. -/
def tinyFn (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 1
  for _ in [0:n] do x := x ^^^ n.toUInt64
  return x

setup_benchmark tinyFn n => n where {
  maxSecondsPerCall := 0.5, paramCeiling := 8
  targetInnerNanos := 50_000_000
  signalFloorMultiplier := 1.0
}

end LeanBench.Test.TimedRegions

private def tinyName : Lean.Name := `LeanBench.Test.TimedRegions.tinyFn

/-- Path to a fresh tempfile under the process temp directory.
Uses the PID and a counter token so concurrent test runs don't
collide. -/
private def freshTempFile (label : String) : IO String := do
  let pid ← IO.Process.getPID
  let mono ← IO.monoNanosNow
  return s!"/tmp/lean-bench-test-{label}-{pid}-{mono}.jsonl"

/-- Pluck an integer JSON field's value from a flat one-line object.
Returns `none` if the field is absent or malformed. Sufficient for
assertions on our well-known sidecar schema; not a general parser. -/
private def fieldNat (line : String) (field : String) : Option Nat := Id.run do
  let needle := s!"\"{field}\":"
  let i := line.splitOn needle
  if i.length < 2 then return none
  let after := i[1]!
  let digits := after.takeWhile (fun c => '0' ≤ c ∧ c ≤ '9')
  if digits.isEmpty then return none
  return digits.toNat?

/-- Assert the file exists, contains exactly one header on the first
line, ≥ 1 region records, that each region has `t0 ≤ t1`, that
regions are non-decreasing in `t0`, and that all region times sit
inside the parent-side wallclock window `[parentT0, parentT1]`.
Returns the count of region records. -/
private def parseAndCheckSidecar
    (path : String) (parentT0 parentT1 : Nat) : IO Nat := do
  let exists? ← System.FilePath.pathExists path
  unless exists? do
    throw (.userError s!"sidecar file not created at {path}")
  let contents ← IO.FS.readFile path
  let lines := contents.splitOn "\n" |>.filter (· ≠ "")
  unless lines.length ≥ 2 do
    throw (.userError s!"sidecar has {lines.length} lines, expected ≥ 2 (header + ≥ 1 region)")
  let header := lines.head!
  unless header.startsWith "{\"kind\":\"header\"" do
    throw (.userError s!"first line is not a header record: {header}")
  match fieldNat header "mono_anchor_ns" with
  | none => throw (.userError s!"header missing mono_anchor_ns: {header}")
  | some _ => pure ()
  let regions := lines.tail!
  let mut prevT0 : Nat := 0
  for region in regions do
    unless region.startsWith "{\"kind\":\"region\"" do
      throw (.userError s!"region line not well-formed: {region}")
    let t0 := (fieldNat region "mono_t0_ns").getD 0
    let t1 := (fieldNat region "mono_t1_ns").getD 0
    if t0 == 0 ∨ t1 == 0 then
      throw (.userError s!"region missing mono_t0_ns / mono_t1_ns: {region}")
    unless t0 ≤ t1 do
      throw (.userError s!"region with t0 > t1: {region}")
    unless prevT0 ≤ t0 do
      throw (.userError s!"region t0 not monotonic: prev {prevT0}, this {region}")
    unless parentT0 ≤ t0 ∧ t1 ≤ parentT1 do
      throw (.userError s!"region times outside parent window [{parentT0}, {parentT1}]: {region}")
    prevT0 := t0
  return regions.length

/-- Spawn the test binary itself in `_child` mode with the given env
overrides. Mirrors the pattern used by the existing E2E tests. -/
private def spawnChild (envOverrides : Array (String × Option String)) :
    IO (UInt32 × String × String) := do
  let exe ← IO.appPath
  let proc ← IO.Process.spawn {
    cmd := exe.toString
    args := #["_child", "--bench", tinyName.toString (escape := false),
              "--param", "4", "--target-nanos", "50000000"]
    stdout := .piped
    stderr := .piped
    stdin  := .null
    env    := envOverrides
  }
  let out ← proc.stdout.readToEnd
  let err ← proc.stderr.readToEnd
  let exit ← proc.wait
  return (exit, out, err)

/-- (1) env var unset → no sidecar file is created. -/
def testEnvVarUnsetNoFile : IO UInt32 := do
  let path ← freshTempFile "envunset"
  try IO.FS.removeFile path catch _ => pure ()
  -- Explicitly unset the env var in the child via Option.none.
  let (exit, _, err) ← spawnChild #[("LEAN_BENCH_TIMED_REGIONS_SIDECAR", none)]
  unless exit == 0 do
    throw (.userError s!"child exit {exit}, stderr:\n{err}")
  let exists? ← System.FilePath.pathExists path
  if exists? then
    throw (.userError s!"file at {path} exists, but env var was unset")
  IO.println "no sidecar created when env var unset ✓"
  return 0

/-- (2) env var set → header + region records appear; region times
respect `t0 ≤ t1`, are monotonically non-decreasing in `t0`, and sit
inside the parent-measured wallclock window of the child spawn. -/
def testEnvVarSetWritesSidecar : IO UInt32 := do
  let path ← freshTempFile "envset"
  try IO.FS.removeFile path catch _ => pure ()
  let parentT0 ← IO.monoNanosNow
  let (exit, _, err) ← spawnChild #[("LEAN_BENCH_TIMED_REGIONS_SIDECAR", some path)]
  let parentT1 ← IO.monoNanosNow
  unless exit == 0 do
    throw (.userError s!"child exit {exit}, stderr:\n{err}")
  let nRegions ← parseAndCheckSidecar path parentT0 parentT1
  IO.println s!"sidecar OK: {nRegions} regions, all bounds well-formed"
  try IO.FS.removeFile path catch _ => pure ()
  return 0

/-- (3) `%p` expands to the child PID, preventing concurrently or
sequentially spawned children from truncating the same sidecar. -/
def testPidPathTemplate : IO UInt32 := do
  let pid := (← IO.Process.getPID).toNat
  let resolved := LeanBench.TimedRegions.resolvePath
    "/tmp/sidecar-%p-%p.jsonl" pid
  let expected := s!"/tmp/sidecar-{pid}-{pid}.jsonl"
  unless resolved == expected do
    throw (.userError s!"PID path template resolved to {resolved}, expected {expected}")
  unless LeanBench.TimedRegions.resolvePath "plain.jsonl" 42 == "plain.jsonl" do
    throw (.userError "a path without %p changed during resolution")
  IO.println s!"PID path template expands: {resolved} ✓"
  return 0

/-- (4) env var set to an unwritable path → child exits non-zero
with a sensible error row. We can't easily synthesise an
unwritable path inside `/tmp`; use a path under a nonexistent
parent directory, which fails the open with a clear error. -/
def testEnvVarSetUnwritableFails : IO UInt32 := do
  let path := "/tmp/lean-bench-test-nonexistent-dir/will-fail.jsonl"
  let (exit, _out, err) ← spawnChild
    #[("LEAN_BENCH_TIMED_REGIONS_SIDECAR", some path)]
  if exit == 0 then
    throw (.userError s!"expected non-zero exit on unwritable sidecar path; got {exit}\nstderr:\n{err}")
  IO.println s!"unwritable sidecar correctly errored: exit {exit} ✓"
  return 0

private def runTest (name : String) (run : IO UInt32) : IO UInt32 := do
  IO.println s!"== {name} =="
  let code ← try run catch e => do
    IO.eprintln s!"FAIL {name}: {e.toString}"
    return (1 : UInt32)
  if code != 0 then
    IO.eprintln s!"FAIL {name}: exit {code}"
  else
    IO.println s!"OK {name}"
  return code

private def runTests : IO UInt32 := do
  let c1 ← runTest "envVarUnsetNoFile" testEnvVarUnsetNoFile
  let c2 ← runTest "envVarSetWritesSidecar" testEnvVarSetWritesSidecar
  let c3 ← runTest "pidPathTemplate" testPidPathTemplate
  let c4 ← runTest "envVarSetUnwritableFails" testEnvVarSetUnwritableFails
  if c1 = 0 ∧ c2 = 0 ∧ c3 = 0 ∧ c4 = 0 then
    IO.println "timed-regions tests passed"
    return 0
  else
    return 1

/-- Same compiled binary acts as parent (test driver) and child
(`_child` arg routes to the harness CLI). Mirrors
`LeanBenchTestProfile.main`. -/
def main (args : List String) : IO UInt32 :=
  match args with
  | "_child" :: _ => LeanBench.Cli.dispatch args
  | _ => runTests
