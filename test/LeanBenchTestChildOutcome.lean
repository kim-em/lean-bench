import LeanBench

/-!
# Parent-side child-outcome classification (issue #2)

`runOneBatch` and `runFixedOneBatch` both fold the post-spawn quartet
`(exit, stdout, stderr, wasKilled)` into a `DataPoint` /
`FixedDataPoint` via the pure `classifyChildOutcome` /
`classifyFixedChildOutcome` helpers. This test pins the failure-mode
contract directly against those classifiers, so the failure branches
are covered without paying the wallclock cost of spawning hundreds of
real subprocesses.

Failure modes pinned here (parametric and fixed):

- exit 0 with empty stdout
- exit 0 with malformed (non-JSON) stdout
- exit 0 with JSON missing required fields
- exit 0 with valid JSONL → row passes through unchanged
- non-zero exit with no stderr
- non-zero exit with stderr (stderr is appended to the error message)
- killed-at-cap (overrides exit code)

The `parseChildRow` parser is also exercised directly on the same
malformed inputs, so a regression in either layer surfaces here.
-/

open LeanBench

/-- Substring containment helper. `String.contains` is `Char`-only. -/
private def containsSub (haystack needle : String) : Bool :=
  ((haystack.splitOn needle).length) > 1

/-- A canonical `ok` JSONL row exactly like `Child.emitRow` produces. -/
private def okJsonlRow : String :=
  "{\"schema_version\":1,\"function\":\"foo.bar\",\"param\":42," ++
  "\"inner_repeats\":1024,\"total_nanos\":3000000," ++
  "\"per_call_nanos\":2929.6875,\"result_hash\":\"0xdeadbeef\"," ++
  "\"status\":\"ok\",\"error\":null}"

/-- A canonical fixed `ok` JSONL row. -/
private def okFixedJsonlRow : String :=
  "{\"schema_version\":1,\"kind\":\"fixed\",\"function\":\"foo.bar\"," ++
  "\"repeat_index\":2,\"total_nanos\":1234567,\"result_hash\":\"0xcafebabe\"," ++
  "\"status\":\"ok\",\"error\":null}"

private def expect (label : String) (cond : Bool) : IO UInt32 := do
  if cond then return 0
  else
    IO.eprintln s!"FAIL: {label}"
    return 1

/-! ## `parseChildRow` direct unit coverage -/

def testParseRejectsEmpty : IO UInt32 := do
  match parseChildRow "" with
  | .ok dp =>
    IO.eprintln s!"expected empty input to fail to parse, got {repr dp}"
    return 1
  | .error _ => return 0

def testParseRejectsMalformedJson : IO UInt32 := do
  match parseChildRow "{not json" with
  | .ok dp =>
    IO.eprintln s!"expected non-JSON to fail to parse, got {repr dp}"
    return 1
  | .error _ => return 0

def testParseRejectsMissingRequiredField : IO UInt32 := do
  -- Drop `"param"` from the canonical row; everything else is
  -- well-formed JSON.
  let row :=
    "{\"schema_version\":1,\"function\":\"foo.bar\"," ++
    "\"inner_repeats\":1024,\"total_nanos\":3000000," ++
    "\"per_call_nanos\":2929.6875,\"status\":\"ok\",\"error\":null}"
  match parseChildRow row with
  | .ok dp =>
    IO.eprintln s!"expected missing-`param` to fail to parse, got {repr dp}"
    return 1
  | .error _ => return 0

def testParseUnknownStatusBecomesError : IO UInt32 := do
  -- Unknown status string must round-trip into `.error`, not silently
  -- coerce to `.ok` (which would mark a broken child as healthy).
  let row :=
    "{\"schema_version\":1,\"function\":\"foo.bar\",\"param\":42," ++
    "\"inner_repeats\":1024,\"total_nanos\":3000000," ++
    "\"per_call_nanos\":2929.6875,\"result_hash\":null," ++
    "\"status\":\"weirdo\",\"error\":null}"
  match parseChildRow row with
  | .error e =>
    IO.eprintln s!"expected unknown-status to parse with `.error` payload, got parse failure: {e}"
    return 1
  | .ok dp =>
    match dp.status with
    | .error msg =>
      if containsSub msg "weirdo" then return 0
      else
        IO.eprintln s!"expected error msg to mention `weirdo`, got: {msg}"
        return 1
    | s =>
      IO.eprintln s!"expected `.error _`, got {repr s}"
      return 1

/-! ## `classifyChildOutcome` (parametric) -/

def testClassifyKilledAtCap : IO UInt32 := do
  -- `wasKilled = true` overrides everything else. Even if the child
  -- somehow emitted a parseable row before being signalled, we report
  -- killedAtCap (the timing is unsound).
  let dp := classifyChildOutcome (param := 7) (exit := 0)
              (stdout := okJsonlRow) (stderrText := "") (wasKilled := true)
  expect s!"killedAtCap (got {repr dp.status})" (dp.status == .killedAtCap)

def testClassifyKilledIgnoresExit : IO UInt32 := do
  -- A non-zero exit code AND a wasKilled flag → still killedAtCap; we
  -- don't want to confuse the user with both signals at once.
  let dp := classifyChildOutcome 0 137 "" "" true
  expect s!"killedAtCap on (exit=137, wasKilled=true), got {repr dp.status}"
    (dp.status == .killedAtCap)

def testClassifyOkRoundtrip : IO UInt32 := do
  let dp := classifyChildOutcome 42 0 okJsonlRow "" false
  expect s!"ok roundtrip: status={repr dp.status} param={dp.param} repeats={dp.innerRepeats}"
    (dp.status == .ok ∧ dp.param == 42 ∧ dp.innerRepeats == 1024
      ∧ dp.totalNanos == 3000000 ∧ dp.resultHash == some 0xdeadbeef)

def testClassifyEmptyStdoutSynthesizesError : IO UInt32 := do
  let dp := classifyChildOutcome 11 0 "" "" false
  match dp.status with
  | .error msg =>
    expect s!"empty-stdout error mentions 'no output': {msg}"
      (containsSub msg "no output" ∧ dp.param == 11 ∧ dp.innerRepeats == 0)
  | s =>
    IO.eprintln s!"expected `.error _`, got {repr s}"
    return 1

def testClassifyMalformedStdoutSynthesizesError : IO UInt32 := do
  let dp := classifyChildOutcome 12 0 "{not json" "" false
  match dp.status with
  | .error msg =>
    expect s!"parse-error message mentions 'parse:': {msg}"
      (containsSub msg "parse:")
  | s =>
    IO.eprintln s!"expected `.error _`, got {repr s}"
    return 1

def testClassifyNonZeroExit : IO UInt32 := do
  let dp := classifyChildOutcome 13 1 "" "" false
  match dp.status with
  | .error msg =>
    expect s!"exit-1 message mentions code: {msg}" (containsSub msg "code 1")
  | s =>
    IO.eprintln s!"expected `.error _`, got {repr s}"
    return 1

def testClassifyNonZeroExitAppendsStderr : IO UInt32 := do
  -- Non-zero exit with stderr → stderr is appended to the synthesized
  -- error message so the user has a hope of debugging.
  let dp := classifyChildOutcome 14 2 "" "boom: cannot allocate" false
  match dp.status with
  | .error msg =>
    expect s!"stderr appended to error: {msg}"
      (containsSub msg "code 2" ∧ containsSub msg "boom: cannot allocate"
        ∧ containsSub msg "stderr:")
  | s =>
    IO.eprintln s!"expected `.error _`, got {repr s}"
    return 1

def testClassifyEmptyStderrNotAppended : IO UInt32 := do
  -- Empty stderr must NOT produce a trailing "; stderr: " noise tag.
  let dp := classifyChildOutcome 15 99 "" "" false
  match dp.status with
  | .error msg =>
    expect s!"no stderr suffix when stderr is empty: {msg}"
      (! containsSub msg "stderr:")
  | s =>
    IO.eprintln s!"expected `.error _`, got {repr s}"
    return 1

/-! ## `classifyFixedChildOutcome` (fixed) -/

def testFixedClassifyKilledAtCap : IO UInt32 := do
  let dp := classifyFixedChildOutcome 0 0 okFixedJsonlRow "" true
  expect s!"fixed killedAtCap: {repr dp.status}" (dp.status == .killedAtCap)

def testFixedClassifyOkRoundtrip : IO UInt32 := do
  let dp := classifyFixedChildOutcome 2 0 okFixedJsonlRow "" false
  expect s!"fixed ok roundtrip: status={repr dp.status} idx={dp.repeatIndex} nanos={dp.totalNanos} hash={dp.resultHash}"
    (dp.status == .ok ∧ dp.repeatIndex == 2 ∧ dp.totalNanos == 1234567
      ∧ dp.resultHash == some 0xcafebabe)

def testFixedClassifyEmptyStdoutSynthesizesError : IO UInt32 := do
  let dp := classifyFixedChildOutcome 3 0 "" "" false
  match dp.status with
  | .error msg =>
    expect s!"fixed empty-stdout error: {msg}"
      (containsSub msg "no output" ∧ dp.repeatIndex == 3)
  | s =>
    IO.eprintln s!"expected `.error _`, got {repr s}"
    return 1

def testFixedClassifyMalformedStdoutSynthesizesError : IO UInt32 := do
  let dp := classifyFixedChildOutcome 4 0 "garbage" "" false
  match dp.status with
  | .error msg =>
    expect s!"fixed parse-error message: {msg}" (containsSub msg "parse:")
  | s =>
    IO.eprintln s!"expected `.error _`, got {repr s}"
    return 1

def testFixedClassifyNonZeroExitAppendsStderr : IO UInt32 := do
  let dp := classifyFixedChildOutcome 5 1 "" "kapow" false
  match dp.status with
  | .error msg =>
    expect s!"fixed stderr appended: {msg}"
      (containsSub msg "code 1" ∧ containsSub msg "kapow")
  | s =>
    IO.eprintln s!"expected `.error _`, got {repr s}"
    return 1

/-! ## Driver -/

def main : IO UInt32 := do
  for (label, t) in
    [ ("parse.empty", testParseRejectsEmpty),
      ("parse.malformedJson", testParseRejectsMalformedJson),
      ("parse.missingField", testParseRejectsMissingRequiredField),
      ("parse.unknownStatus", testParseUnknownStatusBecomesError),
      ("classify.killedAtCap", testClassifyKilledAtCap),
      ("classify.killedIgnoresExit", testClassifyKilledIgnoresExit),
      ("classify.okRoundtrip", testClassifyOkRoundtrip),
      ("classify.emptyStdout", testClassifyEmptyStdoutSynthesizesError),
      ("classify.malformedStdout", testClassifyMalformedStdoutSynthesizesError),
      ("classify.nonZeroExit", testClassifyNonZeroExit),
      ("classify.nonZeroExitWithStderr", testClassifyNonZeroExitAppendsStderr),
      ("classify.emptyStderrNoSuffix", testClassifyEmptyStderrNotAppended),
      ("fixed.killedAtCap", testFixedClassifyKilledAtCap),
      ("fixed.okRoundtrip", testFixedClassifyOkRoundtrip),
      ("fixed.emptyStdout", testFixedClassifyEmptyStdoutSynthesizesError),
      ("fixed.malformedStdout", testFixedClassifyMalformedStdoutSynthesizesError),
      ("fixed.nonZeroExitWithStderr", testFixedClassifyNonZeroExitAppendsStderr) ]
  do
    let code ← t
    if code != 0 then
      IO.eprintln s!"FAIL: {label}"
      return code
    IO.println s!"  ok  {label}"
  IO.println "child-outcome tests passed"
  return 0
