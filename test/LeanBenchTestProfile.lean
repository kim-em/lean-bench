import LeanBench

/-!
# Profile-mode tests (issue #13)

The `profile` subcommand wraps a single child invocation under a
user-supplied profiler command. Real profilers (`perf`, `samply`)
aren't available cross-platform in CI, so we exercise the wrapper
two ways:

1. **Pure unit tests** on `profileTokens` and `buildProfileArgv` —
   the argv assembly logic is the most regression-prone piece of the
   feature and has no IO.
2. **End-to-end smoke** via `/usr/bin/env`, which is present on every
   POSIX host the CI matrix exercises (Linux, macOS) and acts as a
   transparent stub: it just exec's its tail. Skipped on Windows,
   where the CI runs the smoke suite from a different launcher and
   `/usr/bin/env` doesn't exist anyway.

The test deliberately doesn't touch `--cache-mode cold` or any
flag that would change child timing semantics — that's not what
this test is asserting on. The point is "the wrapper command line
gets assembled, exec'd, and the child still emits its JSONL row,"
nothing more.
-/

open LeanBench

namespace LeanBench.Test.Profile

/-- Same trivial benchmark shape as `LeanBenchTestE2E` so we don't
    need a heavyweight registration to test the smoke path. -/
def tinyFn (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 1
  for _ in [0:n] do x := x ^^^ n.toUInt64
  return x

setup_benchmark tinyFn n => n where {
  maxSecondsPerCall := 0.5, paramCeiling := 8, targetInnerNanos := 50_000_000
  signalFloorMultiplier := 1.0
}

end LeanBench.Test.Profile

private def tinyName : Lean.Name := `LeanBench.Test.Profile.tinyFn

/-! ## Unit tests on the argv-assembly helpers -/

/-- `profileTokens` whitespace-splits and drops empty tokens. -/
def testTokensSplits : IO UInt32 := do
  let cases : List (String × Array String) :=
    [ ("perf stat --",            #["perf", "stat", "--"])
    , ("perf record -F 99 -g --", #["perf", "record", "-F", "99", "-g", "--"])
    , ("  samply  record  --  ",  #["samply", "record", "--"])
    , ("",                        #[])
    , ("   ",                     #[])
    , ("env",                     #["env"]) ]
  for (input, expected) in cases do
    let got := LeanBench.profileTokens input
    unless got == expected do
      IO.eprintln s!"FAIL profileTokens({input.quote}): expected {expected}, got {got}"
      return 1
  IO.println "  ok  profileTokens.splits"
  return 0

/-- `buildProfileArgv` returns `none` on empty profiler, otherwise
    `(head, restProfilerArgs ++ exe ++ childArgs)`. -/
def testBuildArgv : IO UInt32 := do
  let exe := "/path/to/exe"
  let childArgs : Array String := #["_child", "--bench", "X"]
  -- Empty profiler → none.
  match LeanBench.buildProfileArgv "" exe childArgs with
  | none => pure ()
  | some (cmd, args) =>
    IO.eprintln s!"FAIL: empty profiler should yield none; got cmd={cmd}, args={args}"
    return 1
  -- Single-token profiler → that token is cmd, exe + childArgs is args.
  match LeanBench.buildProfileArgv "perf" exe childArgs with
  | some (cmd, args) =>
    unless cmd == "perf" do
      IO.eprintln s!"FAIL: cmd should be 'perf'; got {cmd}"
      return 1
    unless args == #[exe] ++ childArgs do
      IO.eprintln s!"FAIL: single-token args wrong; got {args}"
      return 1
  | none =>
    IO.eprintln "FAIL: 'perf' should produce some argv"
    return 1
  -- Multi-token: profiler tokens past the first land before the exe.
  match LeanBench.buildProfileArgv "perf record -F 99 --" exe childArgs with
  | some (cmd, args) =>
    let expected : Array String :=
      #["record", "-F", "99", "--", exe] ++ childArgs
    unless cmd == "perf" do
      IO.eprintln s!"FAIL: cmd should be 'perf'; got {cmd}"
      return 1
    unless args == expected do
      IO.eprintln s!"FAIL: multi-token args wrong; got {args}, expected {expected}"
      return 1
  | none =>
    IO.eprintln "FAIL: multi-token should produce some argv"
    return 1
  IO.println "  ok  buildProfileArgv"
  return 0

/-- `profileChildArgs` produces the same `_child` argv shape that
    `Run.runOneBatch` does (sans `--env-json`). Pin enough of it that
    a regression in the shape — wrong flag name, wrong order — fails
    here rather than at runtime when the child's CLI parser rejects. -/
def testChildArgs : IO UInt32 := do
  match ← findRuntimeEntry tinyName with
  | none =>
    IO.eprintln s!"FAIL: tiny benchmark not registered: {tinyName}"
    return 1
  | some entry =>
    let argv := LeanBench.profileChildArgs entry.spec 5
    -- Must start with `_child` so the parent's dispatcher routes to
    -- child mode, then carry --bench / --param / --target-nanos /
    -- --cache-mode.
    unless argv[0]? == some "_child" do
      IO.eprintln s!"FAIL: argv[0] should be '_child'; got {argv}"
      return 1
    unless argv.contains "--bench" ∧ argv.contains tinyName.toString do
      IO.eprintln s!"FAIL: argv missing --bench {tinyName}; got {argv}"
      return 1
    unless argv.contains "--param" ∧ argv.contains "5" do
      IO.eprintln s!"FAIL: argv missing --param 5; got {argv}"
      return 1
    unless argv.contains "--cache-mode" do
      IO.eprintln s!"FAIL: argv missing --cache-mode; got {argv}"
      return 1
    IO.println "  ok  profileChildArgs"
    return 0

/-! ## End-to-end smoke via `/usr/bin/env` -/

/-- Spawn this binary in `profile` mode wrapped under `/usr/bin/env`
    (a transparent exec stub). Capture stdout, assert it contains the
    child's JSONL row — that's the user-visible "the profiler
    workflow worked" signal. Skipped on Windows; documented in the
    file header.

    Why `/usr/bin/env` and not `env`: hard-code the absolute path so
    PATH peculiarities on `nix`-y CI hosts don't cause the spawn to
    fail with "command not found." Both Ubuntu and macOS images
    ship `/usr/bin/env`. -/
def testProfileSmokeViaEnv : IO UInt32 := do
  if System.Platform.isWindows then
    IO.println "  skip profile.smoke (windows: no /usr/bin/env)"
    return 0
  let exe ← LeanBench.ownExe
  let proc ← IO.Process.output {
    cmd := exe
    args := #[ "profile"
             , tinyName.toString (escape := false)
             , "--profiler", "/usr/bin/env"
             , "--param", "4" ]
    stdin := .null
  }
  -- The child writes one JSONL row to stdout — search for the
  -- distinguishing field. Don't assert exact equality on the row
  -- because timestamps / per-call values vary across hosts.
  let needle := s!"\"function\":\"{tinyName.toString (escape := false)}\""
  if (proc.stdout.splitOn needle).length > 1 then
    IO.println "  ok  profile.smokeViaEnv"
    return 0
  IO.eprintln s!"FAIL: profile smoke: expected stdout to contain {needle}"
  IO.eprintln s!"  exit code: {proc.exitCode}"
  IO.eprintln s!"  stdout: {proc.stdout}"
  IO.eprintln s!"  stderr: {proc.stderr}"
  return 1

/-! ## Driver -/

def runTests : IO UInt32 := do
  let mut anyFail : UInt32 := 0
  for t in
    [ testTokensSplits,
      testBuildArgv,
      testChildArgs,
      testProfileSmokeViaEnv ]
  do
    let code ← t
    if code != 0 then anyFail := 1
  if anyFail == 0 then
    IO.println "profile tests passed"
  return anyFail

/-- Mirror of `LeanBenchTestE2E.main`: same compiled binary acts as
    parent (test driver) and child (single-batch runner spawned by
    the profile path). The `_child` first arg distinguishes them. -/
def main (args : List String) : IO UInt32 :=
  match args with
  | "_child" :: _ => LeanBench.Cli.dispatch args
  | "profile" :: _ => LeanBench.Cli.dispatch args
  | _ => runTests
