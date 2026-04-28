import LeanBench

/-!
# Roundtrip test: child JSONL parses back into a `DataPoint`

The child emits a JSONL row; the parent's `parseChildRow` must
recover an equivalent `DataPoint`. Exercises both `ok`-status and
`error`-status rows.
-/

open LeanBench

/-- A single hand-rolled JSONL row exactly like what `Child.emitRow`
    produces, with all the schema fields in canonical order. -/
def sampleRow : String :=
  "{\"schema_version\":1,\"function\":\"foo.bar\",\"param\":42," ++
  "\"inner_repeats\":1024,\"total_nanos\":3000000," ++
  "\"per_call_nanos\":2929.6875,\"result_hash\":\"0xdeadbeef\"," ++
  "\"status\":\"ok\",\"error\":null}"

/-- A row carrying a full `env` object (issue #11). The parser today
    ignores `env` entirely (it lives on the in-memory result, not the
    `DataPoint`), but the row must still parse cleanly — the env
    object is a nested JSON value with strings, numbers, booleans,
    and explicit nulls, and any laziness in the parser would bite
    here. -/
def sampleRowWithEnv : String :=
  "{\"schema_version\":1,\"kind\":\"parametric\",\"function\":\"foo.bar\"," ++
  "\"param\":7,\"inner_repeats\":2,\"total_nanos\":1000," ++
  "\"per_call_nanos\":500.0,\"cache_mode\":\"warm\"," ++
  "\"result_hash\":\"0xdeadbeef\",\"status\":\"ok\",\"error\":null," ++
  "\"env\":{\"lean_version\":\"4.30.0-rc2\"," ++
  "\"lean_toolchain\":\"leanprover/lean4:v4.30.0-rc2\"," ++
  "\"platform_target\":\"x86_64-unknown-linux-gnu\"," ++
  "\"os\":\"linux\",\"arch\":\"x86_64\"," ++
  "\"cpu_model\":null,\"cpu_cores\":8," ++
  "\"hostname\":\"my-host\",\"exe_name\":\"bench\"," ++
  "\"lean_bench_version\":\"0.1.0\"," ++
  "\"git_commit\":null,\"git_dirty\":null," ++
  "\"timestamp_unix_ms\":1714316040000," ++
  "\"timestamp_iso\":\"2026-04-28T12:34:00Z\"}}"

def main : IO UInt32 := do
  match LeanBench.parseChildRow sampleRow with
  | .error e =>
    IO.eprintln s!"parse failure: {e}"
    return 1
  | .ok dp =>
    -- Spot-check the fields we set above.
    let expected : DataPoint :=
      { param := 42, innerRepeats := 1024, totalNanos := 3000000
      , perCallNanos := 2929.6875
      , resultHash := some 0xdeadbeef
      , status := .ok }
    let okExpected :=
      dp.param == expected.param ∧
      dp.innerRepeats == expected.innerRepeats ∧
      dp.totalNanos == expected.totalNanos ∧
      dp.resultHash == expected.resultHash ∧
      dp.status == expected.status
    unless okExpected do
      IO.eprintln s!"roundtrip mismatch: got {repr dp}"
      return 1
  match LeanBench.parseChildRow sampleRowWithEnv with
  | .error e =>
    IO.eprintln s!"env-row parse failure: {e}"
    return 1
  | .ok dp =>
    unless dp.param == 7 ∧ dp.innerRepeats == 2 ∧ dp.totalNanos == 1000 do
      IO.eprintln s!"env-row mismatch: {repr dp}"
      return 1
  IO.println "roundtrip OK"
  return 0
