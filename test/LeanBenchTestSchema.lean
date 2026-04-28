import LeanBench

/-!
# Schema-stability tests (issue #14)

Pin the wire format documented in `doc/schema.md` so accidental
changes to required keys, the version constant, or compatibility
expectations break this test.

Two perspectives:

- **Producer.** A child process for a parametric and a fixed
  benchmark must emit a JSONL row that contains the canonical
  required keys plus today's optional keys, with `schema_version`
  matching `Schema.schemaVersion`.
- **Consumer.** `parseChildRow` and `parseFixedChildRow` must accept
  rows with unknown extra fields, accept rows with missing
  optional fields, reject rows with missing required fields, and
  reject rows whose `schema_version` is newer than the reader.
-/

open LeanBench

namespace LeanBench.Test.Schema

/-- A trivial parametric benchmark. The body just returns `n`; we're
    pinning the schema, not the workload. -/
def schemaProbe (n : Nat) : UInt64 := n.toUInt64

setup_benchmark schemaProbe n => n + 1 where {
  paramCeiling := 4
  paramFloor := 0
  maxSecondsPerCall := 1.0
}

/-- A fixed-benchmark counterpart so the fixed-emit path is exercised
    by `verify`. -/
def schemaFixedProbe : UInt64 := 7

setup_fixed_benchmark schemaFixedProbe where { repeats := 1, warmup := false }

end LeanBench.Test.Schema

private def schemaProbeName : Lean.Name := `LeanBench.Test.Schema.schemaProbe
private def schemaFixedProbeName : Lean.Name := `LeanBench.Test.Schema.schemaFixedProbe

/-- Substring containment helper. -/
private def containsSub (haystack needle : String) : Bool :=
  ((haystack.splitOn needle).length) > 1

/-- Spawn the running binary in `_child` mode and return its stdout. -/
private def spawnChild (args : List String) : IO (UInt32 × String) := do
  let exe := (← IO.appPath).toString
  let child ← IO.Process.spawn {
    cmd := exe
    args := args.toArray
    stdout := .piped
    stderr := .piped
    stdin := .null
  }
  let stdout ← child.stdout.readToEnd
  let exit ← child.wait
  return (exit, stdout.trimAscii.toString)

/-- Look up a JSON object's key set, sorted ascending so the
    fixture-comparison order is deterministic. -/
private def objKeys (j : Lean.Json) : Array String :=
  match j with
  | .obj kvs =>
    let xs : Array String := kvs.toArray.map Prod.fst
    xs.qsort (· < ·)
  | _ => #[]

/-- Assert that every key in `required` is present in `actual`.
    Returns the missing key names, if any. -/
private def missingKeys (actual : Array String) (required : Array String) :
    Array String :=
  required.filter (fun k => !actual.contains k)

private def expect (label : String) (cond : Bool) : IO UInt32 := do
  if cond then return 0
  IO.eprintln s!"FAIL: {label}"
  return 1

/-! ## Producer-side: an emitted row carries the documented keys -/

/-- Sort an array of strings ascending. Local helper so the
    expected-key fixtures below stay readable. -/
private def sortKeys (xs : Array String) : Array String := xs.qsort (· < ·)

/-- The exact set of keys today's parametric writer emits. Any
    additional optional field landing in #6/#9/#11 must update both
    this fixture and `Schema.optionalParametricKeys`/
    `optionalCommonKeys`, and the producer test below catches
    missing edits. -/
private def expectedParametricKeys : Array String := sortKeys (
  Schema.requiredCommonKeys ++ Schema.requiredParametricKeys ++
  Schema.optionalCommonKeys ++ Schema.optionalParametricKeys)

/-- The exact set of keys today's fixed writer emits. -/
private def expectedFixedKeys : Array String := sortKeys (
  Schema.requiredCommonKeys ++ Schema.requiredFixedKeys ++
  Schema.optionalCommonKeys)

/-- Spawn the child for a parametric run and check the resulting
    JSONL row's keys against the canonical fixture. -/
def testParametricEmitKeys : IO UInt32 := do
  let (exit, line) ← spawnChild
    ["_child", "--bench", schemaProbeName.toString (escape := false),
     "--param", "1", "--target-nanos", "1000000"]
  if exit != 0 then
    IO.eprintln s!"parametric child exited {exit}; stdout: {line}"
    return 1
  match Lean.Json.parse line with
  | .error e =>
    IO.eprintln s!"parametric row failed to parse: {e}\nrow: {line}"
    return 1
  | .ok j =>
    let actual := objKeys j
    -- Pin the exact emitted-key set so silently dropping or adding
    -- a field shows up in the diff.
    unless actual == expectedParametricKeys do
      IO.eprintln s!"parametric row keys mismatch:\n  got      {actual}\n  expected {expectedParametricKeys}"
      return 1
    -- `schema_version` must equal the constant.
    match j.getObjValAs? Nat "schema_version" with
    | .ok v =>
      unless v == Schema.schemaVersion do
        IO.eprintln s!"emitted schema_version={v}, expected {Schema.schemaVersion}"
        return 1
    | .error _ =>
      IO.eprintln "parametric row missing schema_version"
      return 1
    -- `kind` must be `"parametric"` (the v1 contract).
    match j.getObjValAs? String "kind" with
    | .ok k =>
      unless k == Schema.kindParametric do
        IO.eprintln s!"emitted kind={k}, expected {Schema.kindParametric}"
        return 1
    | .error _ =>
      IO.eprintln "parametric row missing kind discriminator"
      return 1
    -- `status` must be one of the documented status strings.
    match j.getObjValAs? String "status" with
    | .ok s =>
      unless Schema.statusStrings.contains s do
        IO.eprintln s!"emitted status={s}, not in documented set {Schema.statusStrings}"
        return 1
    | .error _ =>
      IO.eprintln "parametric row missing status"
      return 1
    return 0

def testFixedEmitKeys : IO UInt32 := do
  let (exit, line) ← spawnChild
    ["_child", "--bench", schemaFixedProbeName.toString (escape := false),
     "--fixed", "--repeat-index", "0"]
  if exit != 0 then
    IO.eprintln s!"fixed child exited {exit}; stdout: {line}"
    return 1
  match Lean.Json.parse line with
  | .error e =>
    IO.eprintln s!"fixed row failed to parse: {e}\nrow: {line}"
    return 1
  | .ok j =>
    let actual := objKeys j
    unless actual == expectedFixedKeys do
      IO.eprintln s!"fixed row keys mismatch:\n  got      {actual}\n  expected {expectedFixedKeys}"
      return 1
    -- `kind == "fixed"` is mandatory on the fixed side.
    match j.getObjValAs? String "kind" with
    | .ok k =>
      unless k == Schema.kindFixed do
        IO.eprintln s!"emitted kind={k}, expected {Schema.kindFixed}"
        return 1
    | .error _ =>
      IO.eprintln "fixed row missing kind discriminator"
      return 1
    return 0

/-! ## Producer-side: pin the canonical key sets

If `Schema.requiredCommonKeys` etc. change, this test catches it
before any external tool that depends on the wire format does.
Adding a new key here is a one-line edit; removing a required key
needs a `schema_version` bump. -/

def testCanonicalKeyConstants : IO UInt32 := do
  let okCommon :=
    Schema.requiredCommonKeys ==
      #["schema_version", "function", "status", "total_nanos"]
  let okParametric :=
    Schema.requiredParametricKeys == #["param", "inner_repeats"]
  let okFixed :=
    Schema.requiredFixedKeys == #["repeat_index"]
  let okOptCommon :=
    Schema.optionalCommonKeys == #["kind", "result_hash", "error"]
  let okOptParametric :=
    Schema.optionalParametricKeys == #["per_call_nanos", "cache_mode"]
  let okStatus :=
    Schema.statusStrings ==
      #["ok", "timed_out", "killed_at_cap", "error"]
  let okVersion := Schema.schemaVersion == 1
  let okKindStrings :=
    Schema.kindParametric == "parametric" ∧ Schema.kindFixed == "fixed"
  expect "canonical key sets match docs"
    (okCommon ∧ okParametric ∧ okFixed ∧ okOptCommon ∧
     okOptParametric ∧ okStatus ∧ okVersion ∧ okKindStrings)

/-! ## Consumer-side: parse tolerance -/

def testParseAcceptsExtraKey : IO UInt32 := do
  -- A future version may add `alloc_bytes` (issue #6). Today's
  -- parser must ignore it rather than failing.
  let row :=
    "{\"schema_version\":1,\"kind\":\"parametric\",\"function\":\"foo.bar\"," ++
    "\"param\":42,\"inner_repeats\":1024,\"total_nanos\":3000000," ++
    "\"per_call_nanos\":2929.6875,\"result_hash\":\"0xdeadbeef\"," ++
    "\"status\":\"ok\",\"error\":null," ++
    "\"alloc_bytes\":12345,\"future_field\":\"hello\"}"
  match parseChildRow row with
  | .error e =>
    IO.eprintln s!"expected extra-key row to parse, got error: {e}"
    return 1
  | .ok dp =>
    expect s!"extra-key parametric row parses cleanly: {repr dp}"
      (dp.param == 42 ∧ dp.innerRepeats == 1024 ∧ dp.totalNanos == 3000000)

def testParseAcceptsMissingOptional : IO UInt32 := do
  -- Drop `result_hash` and `error` (both optional) and ensure the
  -- parser still succeeds. `kind` is also optional in v1.
  let row :=
    "{\"schema_version\":1,\"function\":\"foo.bar\"," ++
    "\"param\":7,\"inner_repeats\":2,\"total_nanos\":1000," ++
    "\"per_call_nanos\":500.0,\"status\":\"ok\"}"
  match parseChildRow row with
  | .error e =>
    IO.eprintln s!"expected missing-optional row to parse, got error: {e}"
    return 1
  | .ok dp =>
    expect s!"missing-optional parametric row: {repr dp}"
      (dp.param == 7 ∧ dp.innerRepeats == 2 ∧ dp.totalNanos == 1000
        ∧ dp.resultHash == none ∧ dp.status == .ok)

def testParseRecomputesPerCallWhenAbsent : IO UInt32 := do
  -- `per_call_nanos` is documented as derived; readers MAY recompute
  -- it from `total_nanos / inner_repeats`. Pin that fallback.
  let row :=
    "{\"schema_version\":1,\"function\":\"foo.bar\"," ++
    "\"param\":7,\"inner_repeats\":2,\"total_nanos\":1000," ++
    "\"status\":\"ok\"}"
  match parseChildRow row with
  | .error e =>
    IO.eprintln s!"missing-per_call row failed to parse: {e}"
    return 1
  | .ok dp =>
    -- 1000 / 2 = 500.0 exactly.
    expect s!"per_call recomputed: {dp.perCallNanos}" (dp.perCallNanos == 500.0)

def testParseRejectsFutureVersion : IO UInt32 := do
  -- `schema_version > Schema.schemaVersion` must fail with a
  -- message that names the unsupported version. Silent best-effort
  -- reads of newer rows would silently corrupt downstream analysis.
  let row :=
    "{\"schema_version\":99,\"kind\":\"parametric\",\"function\":\"foo.bar\"," ++
    "\"param\":1,\"inner_repeats\":1,\"total_nanos\":1," ++
    "\"per_call_nanos\":1.0,\"status\":\"ok\"}"
  match parseChildRow row with
  | .ok dp =>
    IO.eprintln s!"expected future-version row to fail, got {repr dp}"
    return 1
  | .error msg =>
    expect s!"future-version error mentions '99': {msg}"
      (containsSub msg "99")

def testParseToleratesMissingVersion : IO UInt32 := do
  -- Hand-rolled fixtures or pre-versioning rows may omit
  -- `schema_version`. The parser tolerates that and treats it as v1.
  let row :=
    "{\"function\":\"foo.bar\"," ++
    "\"param\":3,\"inner_repeats\":4,\"total_nanos\":12," ++
    "\"per_call_nanos\":3.0,\"status\":\"ok\"}"
  match parseChildRow row with
  | .error e =>
    IO.eprintln s!"missing-version row failed to parse: {e}"
    return 1
  | .ok dp =>
    expect s!"missing-version parses: {repr dp}"
      (dp.param == 3 ∧ dp.innerRepeats == 4)

/-! ## Fixed-row consumer-side mirrors -/

def testFixedParseAcceptsExtraKey : IO UInt32 := do
  let row :=
    "{\"schema_version\":1,\"kind\":\"fixed\",\"function\":\"foo.bar\"," ++
    "\"repeat_index\":0,\"total_nanos\":1234567,\"result_hash\":\"0xcafe\"," ++
    "\"status\":\"ok\",\"error\":null,\"alloc_bytes\":42}"
  match parseFixedChildRow row with
  | .error e =>
    IO.eprintln s!"fixed extra-key row failed to parse: {e}"
    return 1
  | .ok dp =>
    expect s!"fixed extra-key parses: {repr dp}"
      (dp.repeatIndex == 0 ∧ dp.totalNanos == 1234567 ∧ dp.status == .ok)

def testFixedParseRejectsFutureVersion : IO UInt32 := do
  let row :=
    "{\"schema_version\":42,\"kind\":\"fixed\",\"function\":\"foo.bar\"," ++
    "\"repeat_index\":0,\"total_nanos\":1,\"status\":\"ok\"}"
  match parseFixedChildRow row with
  | .ok dp =>
    IO.eprintln s!"expected fixed future-version row to fail, got {repr dp}"
    return 1
  | .error msg =>
    expect s!"fixed future-version mentions '42': {msg}" (containsSub msg "42")

def testFixedParseAcceptsMissingOptional : IO UInt32 := do
  -- Drop `result_hash` and `error`. `kind` stays — fixed parser
  -- requires it to discriminate from a misrouted parametric row.
  let row :=
    "{\"schema_version\":1,\"kind\":\"fixed\",\"function\":\"foo.bar\"," ++
    "\"repeat_index\":2,\"total_nanos\":99,\"status\":\"ok\"}"
  match parseFixedChildRow row with
  | .error e =>
    IO.eprintln s!"fixed missing-optional row failed: {e}"
    return 1
  | .ok dp =>
    expect s!"fixed missing-optional parses: {repr dp}"
      (dp.repeatIndex == 2 ∧ dp.totalNanos == 99 ∧ dp.resultHash == none)

/-! ## Negative coverage: missing required fields, wrong kind, old version -/

/-- Helper: assert that `parseChildRow` rejects `row` and that the
    error message mentions `expectedSubstring`. -/
private def expectParseError (row expectedSubstring label : String) :
    IO UInt32 := do
  match parseChildRow row with
  | .ok dp =>
    IO.eprintln s!"FAIL ({label}): expected parse error, got {repr dp}"
    return 1
  | .error msg =>
    if containsSub msg expectedSubstring then return 0
    else
      IO.eprintln s!"FAIL ({label}): expected error mentioning '{expectedSubstring}', got: {msg}"
      return 1

private def expectFixedParseError (row expectedSubstring label : String) :
    IO UInt32 := do
  match parseFixedChildRow row with
  | .ok dp =>
    IO.eprintln s!"FAIL ({label}): expected parse error, got {repr dp}"
    return 1
  | .error msg =>
    if containsSub msg expectedSubstring then return 0
    else
      IO.eprintln s!"FAIL ({label}): expected error mentioning '{expectedSubstring}', got: {msg}"
      return 1

/-- A canonical valid parametric row, used as the basis for "drop
    one required key" negative tests. -/
private def baseValidParametricRow : String :=
  "{\"schema_version\":1,\"kind\":\"parametric\",\"function\":\"foo.bar\"," ++
  "\"param\":1,\"inner_repeats\":1,\"total_nanos\":1," ++
  "\"per_call_nanos\":1.0,\"result_hash\":null," ++
  "\"status\":\"ok\",\"error\":null}"

private def baseValidFixedRow : String :=
  "{\"schema_version\":1,\"kind\":\"fixed\",\"function\":\"foo.bar\"," ++
  "\"repeat_index\":0,\"total_nanos\":1,\"result_hash\":null," ++
  "\"status\":\"ok\",\"error\":null}"

def testParseRejectsExplicitOldVersion : IO UInt32 :=
  expectParseError
    "{\"schema_version\":0,\"kind\":\"parametric\",\"function\":\"foo.bar\",\"param\":1,\"inner_repeats\":1,\"total_nanos\":1,\"status\":\"ok\"}"
    "0" "parse.explicitOldVersion"

def testFixedParseRejectsExplicitOldVersion : IO UInt32 :=
  expectFixedParseError
    "{\"schema_version\":0,\"kind\":\"fixed\",\"function\":\"foo.bar\",\"repeat_index\":0,\"total_nanos\":1,\"status\":\"ok\"}"
    "0" "fixed.explicitOldVersion"

def testParseRejectsWrongKind : IO UInt32 :=
  -- A `kind:"fixed"` row must be rejected by the parametric parser.
  expectParseError
    "{\"schema_version\":1,\"kind\":\"fixed\",\"function\":\"foo.bar\",\"param\":1,\"inner_repeats\":1,\"total_nanos\":1,\"status\":\"ok\"}"
    "kind" "parse.wrongKind"

def testFixedParseRejectsWrongKind : IO UInt32 :=
  -- Symmetric: `kind:"parametric"` (or missing, which defaults to
  -- parametric) must not parse via the fixed parser.
  expectFixedParseError
    "{\"schema_version\":1,\"kind\":\"parametric\",\"function\":\"foo.bar\",\"repeat_index\":0,\"total_nanos\":1,\"status\":\"ok\"}"
    "kind" "fixed.wrongKind"

def testFixedParseRejectsMissingKind : IO UInt32 :=
  -- Missing `kind` defaults to parametric per the back-compat rule;
  -- the fixed parser must reject that.
  expectFixedParseError
    "{\"schema_version\":1,\"function\":\"foo.bar\",\"repeat_index\":0,\"total_nanos\":1,\"status\":\"ok\"}"
    "kind" "fixed.missingKind"

/-- Drop each required field individually from a known-good
    parametric row and assert the parser refuses each variant. The
    fixture used for `Lean.Json` munging is built up below. -/
def testParseRejectsMissingRequired : IO UInt32 := do
  let droppers : List (String × String) :=
    [ ("\"function\":\"foo.bar\",", "function")
    , ("\"param\":1,",              "param")
    , ("\"inner_repeats\":1,",      "inner_repeats")
    , ("\"total_nanos\":1,",        "total_nanos")
    , ("\"status\":\"ok\",",        "status") ]
  for (substr, fieldName) in droppers do
    -- Build a row missing exactly this required field.
    let parts := baseValidParametricRow.splitOn substr
    if parts.length != 2 then
      IO.eprintln s!"FAIL: expected unique occurrence of '{substr}' in fixture"
      return 1
    let mutilated := String.intercalate "" parts
    match parseChildRow mutilated with
    | .ok dp =>
      IO.eprintln s!"FAIL (parse.missing.{fieldName}): expected parse failure, got {repr dp}"
      return 1
    | .error _ => pure ()
  return 0

def testFixedParseRejectsMissingRequired : IO UInt32 := do
  let droppers : List (String × String) :=
    [ ("\"function\":\"foo.bar\",", "function")
    , ("\"repeat_index\":0,",       "repeat_index")
    , ("\"total_nanos\":1,",        "total_nanos")
    , ("\"status\":\"ok\",",        "status") ]
  for (substr, fieldName) in droppers do
    let parts := baseValidFixedRow.splitOn substr
    if parts.length != 2 then
      IO.eprintln s!"FAIL: expected unique occurrence of '{substr}' in fixed fixture"
      return 1
    let mutilated := String.intercalate "" parts
    match parseFixedChildRow mutilated with
    | .ok dp =>
      IO.eprintln s!"FAIL (fixed.missing.{fieldName}): expected parse failure, got {repr dp}"
      return 1
    | .error _ => pure ()
  return 0

/-! ## Driver -/

def runTests : IO UInt32 := do
  for (label, t) in
    [ ("canonicalKeyConstants",      testCanonicalKeyConstants)
    , ("emit.parametric",            testParametricEmitKeys)
    , ("emit.fixed",                 testFixedEmitKeys)
    , ("parse.extraKey",             testParseAcceptsExtraKey)
    , ("parse.missingOptional",      testParseAcceptsMissingOptional)
    , ("parse.recomputesPerCall",    testParseRecomputesPerCallWhenAbsent)
    , ("parse.futureVersionRejected",testParseRejectsFutureVersion)
    , ("parse.explicitOldVersion",   testParseRejectsExplicitOldVersion)
    , ("parse.missingVersion",       testParseToleratesMissingVersion)
    , ("parse.wrongKind",            testParseRejectsWrongKind)
    , ("parse.missingRequired",      testParseRejectsMissingRequired)
    , ("fixed.extraKey",             testFixedParseAcceptsExtraKey)
    , ("fixed.futureVersionRejected",testFixedParseRejectsFutureVersion)
    , ("fixed.explicitOldVersion",   testFixedParseRejectsExplicitOldVersion)
    , ("fixed.missingOptional",      testFixedParseAcceptsMissingOptional)
    , ("fixed.wrongKind",            testFixedParseRejectsWrongKind)
    , ("fixed.missingKind",          testFixedParseRejectsMissingKind)
    , ("fixed.missingRequired",      testFixedParseRejectsMissingRequired) ]
  do
    let code ← t
    if code != 0 then
      IO.eprintln s!"FAIL: {label}"
      return code
    IO.println s!"  ok  {label}"
  IO.println "schema tests passed"
  return 0

/-- The same compiled binary acts as parent (test driver) and child
    (the `_child` first arg distinguishes them). The parametric and
    fixed emit-side tests spawn this binary in child mode against
    `schemaProbe` / `schemaFixedProbe`. -/
def main (args : List String) : IO UInt32 :=
  match args with
  | "_child" :: _ => LeanBench.Cli.dispatch args
  | _ => runTests
