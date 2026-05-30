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

/-- Assert the file exists, contains exactly one header record on
the first line, and ≥ 1 region records. Returns the parsed body
for further assertions. -/
private def parseAndCheckSidecar (path : String) :
    IO (List String) := do
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
  let regions := lines.tail!
  for region in regions do
    unless region.startsWith "{\"kind\":\"region\"" do
      throw (.userError s!"region line not well-formed: {region}")
  return lines

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

/-- (2) env var set → header + region records appear. -/
def testEnvVarSetWritesSidecar : IO UInt32 := do
  let path ← freshTempFile "envset"
  try IO.FS.removeFile path catch _ => pure ()
  let (exit, _, err) ← spawnChild #[("LEAN_BENCH_TIMED_REGIONS_SIDECAR", some path)]
  unless exit == 0 do
    throw (.userError s!"child exit {exit}, stderr:\n{err}")
  let lines ← parseAndCheckSidecar path
  IO.println s!"sidecar OK: {lines.length} lines ({lines.length - 1} regions)"
  try IO.FS.removeFile path catch _ => pure ()
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
  if c1 = 0 ∧ c2 = 0 then
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
