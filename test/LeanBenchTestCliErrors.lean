import LeanBench

/-!
# CLI argument-validation tests (issue #2)

`LeanBenchTestConfig` covers the override-flag pipeline and pins typo
rejection for one bad spelling. This file goes wider on the parser-
and dispatch-level error paths so a regression in CLI ergonomics
(missing positionals, unknown subcommands, default-subcommand
dispatch, handler errors on unregistered names) breaks the build.

What's pinned:

- Unknown subcommand → parser rejects.
- Missing required positional `name` on `run` → parser rejects.
- `--param-floor` rejects negative input (`Nat` parser).
- `--param-ceiling` rejects non-numeric input.
- `--max-seconds-per-call` rejects non-numeric input.
- `dispatch []` defaults to `list` and exits 0.
- `compare` with zero names → handler exits non-zero with the
  documented error message.
- `run NAME` with an unregistered `NAME` → handler exits non-zero.

Handler-level tests drive `Cli.dispatch` (which calls
`topCmd.validate`); the test process inherits the eprintln that the
handler writes, but we only assert on the exit code so the noise
doesn't make the test brittle.
-/

open LeanBench

private def containsSub (haystack needle : String) : Bool :=
  ((haystack.splitOn needle).length) > 1

/-! ## Parser-level rejection -/

/-- Drive `topCmd.process` and assert it errors. The matched message
    is checked against `expectedSub` so we don't depend on the exact
    wording of an internal Cli error template. -/
private def expectParserError (label : String) (argv : List String)
    (expectedSub : String) : IO UInt32 := do
  match LeanBench.Cli.topCmd.process argv with
  | .ok _ =>
    IO.eprintln s!"FAIL: {label}: expected parser to reject {argv}"
    return 1
  | .error (_, msg) =>
    if containsSub msg expectedSub then
      IO.println s!"  ok  {label}"
      return 0
    IO.eprintln s!"FAIL: {label}: expected msg to mention {expectedSub.quote}, got: {msg}"
    return 1

def testUnknownSubcommand : IO UInt32 :=
  expectParserError "parser.unknownSubcommand" ["totally-not-a-command"] "totally-not-a-command"

/-- `run` with no names and no filters: the handler returns 1 with a
    helpful error. (Before issue #10, `name` was a required positional
    arg and the parser would reject; now it's variadic and the handler
    checks.) -/
def testRunMissingName : IO UInt32 := do
  let code ← LeanBench.Cli.dispatch ["run"]
  if code == 1 then
    IO.println "  ok  dispatch.runMissingName"
    return 0
  IO.eprintln s!"FAIL: dispatch.runMissingName: expected exit 1, got {code}"
  return 1

def testNegativeNatRejected : IO UInt32 :=
  -- `--param-floor` is typed `Nat`; the parser must reject negatives,
  -- not silently coerce to 0 or wrap to a giant unsigned value.
  expectParserError "parser.negativeNatRejected"
    ["run", "anything", "--param-floor", "-5"]
    "param-floor"

def testNonNumericNatRejected : IO UInt32 :=
  expectParserError "parser.nonNumericNatRejected"
    ["run", "anything", "--param-ceiling", "huge"]
    "param-ceiling"

def testNonNumericFloatRejected : IO UInt32 :=
  expectParserError "parser.nonNumericFloatRejected"
    ["run", "anything", "--max-seconds-per-call", "soonish"]
    "max-seconds-per-call"

/-! ## Dispatch-level behaviour -/

/-- `dispatch []` must not crash and must dispatch as `list`. We can't
    capture stdout cleanly here, but the exit code distinguishes
    "happy path" from "argument validation error". -/
def testDispatchEmptyDefaultsToList : IO UInt32 := do
  let code ← LeanBench.Cli.dispatch []
  if code == 0 then
    IO.println "  ok  dispatch.emptyDefaultsToList"
    return 0
  IO.eprintln s!"FAIL: dispatch.emptyDefaultsToList: expected 0, got {code}"
  return 1

/-- `compare` with zero benchmark names: the handler eprints the
    documented message and returns 1. -/
def testCompareNoNames : IO UInt32 := do
  -- The handler writes "compare: need at least two benchmark names"
  -- to stderr and returns 1. We don't capture stderr; just assert
  -- the exit code so the test is robust to wording tweaks.
  let code ← LeanBench.Cli.dispatch ["compare"]
  if code == 1 then
    IO.println "  ok  dispatch.compareNoNames"
    return 0
  IO.eprintln s!"FAIL: dispatch.compareNoNames: expected exit 1, got {code}"
  return 1

/-- `run` against an unregistered name: handler returns 1. -/
def testRunUnregistered : IO UInt32 := do
  let code ← LeanBench.Cli.dispatch ["run", "definitelyNotRegisteredXYZ"]
  if code == 1 then
    IO.println "  ok  dispatch.runUnregistered"
    return 0
  IO.eprintln s!"FAIL: dispatch.runUnregistered: expected exit 1, got {code}"
  return 1

/-- `verify` with an unregistered name: handler returns 1. The
    underlying `verify` call throws a userError on unknown names; the
    CLI catches it, prints, and returns 1. -/
def testVerifyUnregistered : IO UInt32 := do
  let code ← LeanBench.Cli.dispatch ["verify", "definitelyNotRegisteredXYZ"]
  if code == 1 then
    IO.println "  ok  dispatch.verifyUnregistered"
    return 0
  IO.eprintln s!"FAIL: dispatch.verifyUnregistered: expected exit 1, got {code}"
  return 1

/-! ## Driver -/

def main : IO UInt32 := do
  let mut anyFail : UInt32 := 0
  for t in
    [ testUnknownSubcommand,
      testRunMissingName,
      testNegativeNatRejected,
      testNonNumericNatRejected,
      testNonNumericFloatRejected,
      testDispatchEmptyDefaultsToList,
      testCompareNoNames,
      testRunUnregistered,
      testVerifyUnregistered ]
  do
    let code ← t
    if code != 0 then anyFail := 1
  if anyFail == 0 then
    IO.println "cli-errors tests passed"
  return anyFail
