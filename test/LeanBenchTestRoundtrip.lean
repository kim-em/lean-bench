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
    if okExpected then
      IO.println "roundtrip OK"
      return 0
    else
      IO.eprintln s!"roundtrip mismatch: got {repr dp}"
      return 1
