import LeanBench.Core
import LeanBench.Env
import LeanBench.Run
import LeanBench.Schema

/-!
# `LeanBench.Suite` — CI-budgeted suite scheduler (issue #9)

Runs the registered benchmark catalogue inside a fixed wall-clock
budget. Walks the parametric registry first, then the fixed
registry; for each entry, checks whether enough budget remains to
start it. If yes, the benchmark runs with a deadline-aware ladder
that aborts mid-walk once the budget is exhausted; if no, the entry
is recorded as `.skipped` so the report and exported JSONL
explicitly account for the deferred work.

The bound on total wall time is `totalSeconds + maxSecondsPerCall +
killGraceMs + a few hundred ms of process-spawn slack`: at most one
in-flight batch can outlive the deadline, and the kill-on-cap
machinery in `runOneBatch` / `runFixedOneBatch` bounds that slack.
This is the "predictable bound" #9's acceptance criteria asks for.
-/

namespace LeanBench
namespace Suite

/-! ## Pure scheduler primitives -/

/-- Convert a wall-clock budget in seconds to monotonic-clock nanos.
    Clamps tiny / negative values to `0`. The caller adds the result
    to a previously-captured `IO.monoNanosNow` to get an absolute
    deadline. Pinned in its own def so the test suite can drive the
    scheduler logic without arithmetic spread across call sites. -/
def secondsToNanos (s : Float) : Nat :=
  if s ≤ 0.0 then 0 else (s * 1.0e9).toUInt64.toNat

/-- Decide whether the current monotonic clock has enough remaining
    budget to start another benchmark. Returns `true` when the entry
    should run, `false` when it should be skipped. The threshold is
    `cfg.minPerBenchmarkSeconds` — below it, starting another
    benchmark only burns the remaining budget on a measurement that
    will almost-immediately be aborted by the deadline check anyway. -/
def shouldStart (cfg : SuiteBudgetConfig) (deadline now : Nat) : Bool :=
  -- Nat subtraction is saturating (truncates at zero), so we route through
  -- `Int` to detect "deadline already past" cleanly.
  let remaining : Int := (Int.ofNat deadline) - (Int.ofNat now)
  let minNanos : Int := Int.ofNat (secondsToNanos cfg.minPerBenchmarkSeconds)
  remaining ≥ minNanos

/-! ## Suite scheduler -/

/-- Run the registered benchmark catalogue inside a wall-clock budget.

The scheduler walks the parametric registry first and then the
fixed registry. For each entry:

1. Check `shouldStart`. If false, push a `.skipped` `SuiteEntry` and
   continue.
2. Otherwise dispatch through `runBenchmark` / `runFixedBenchmark`
   with `deadline?` set, so the per-benchmark ladder aborts if the
   suite budget runs out mid-walk.
3. Record the wall time spent on the benchmark for the report.

The optional `override` / `fixedOverride` apply uniformly to every
benchmark in the suite (mirroring `compare`). They're useful for CI
configurations that want to tighten `--max-seconds-per-call` across
the board, for example.

Validation errors from individual benchmarks are caught and turned
into skip entries so a single invalid `BenchmarkConfig` doesn't
poison the whole suite. -/
def runSuiteWithBudget (cfg : SuiteBudgetConfig)
    (override : ConfigOverride := {})
    (fixedOverride : FixedConfigOverride := {}) :
    IO SuiteReport := do
  let pEntries ← allRuntimeEntries
  let fEntries ← allFixedRuntimeEntries
  let totalNanos : Nat := secondsToNanos cfg.totalSeconds
  let t0 ← IO.monoNanosNow
  let deadline : Nat := t0 + totalNanos
  let mut entries : Array SuiteEntry := #[]
  -- Standardised reason strings on the skip rows. The exact text is
  -- semi-stable: downstream tooling MAY filter on
  -- `budget_status == "skipped"` plus a substring match against
  -- "budget exhausted" / "deadline reached" / "runBenchmark failed",
  -- but the canonical machine signal is `budget_status` itself.
  let budgetExhaustedMsg : String := "skipped: budget exhausted before start"
  let deadlineDuringRunMsg : String :=
    "skipped: deadline reached before any measurement landed"
  for entry in pEntries do
    let now ← IO.monoNanosNow
    if shouldStart cfg deadline now then
      let benchStart ← IO.monoNanosNow
      let resultE ← (runBenchmark entry.spec.name override (deadline? := some deadline)).toBaseIO
      let benchEnd ← IO.monoNanosNow
      let dt : Float := (benchEnd - benchStart).toFloat / 1.0e9
      match resultE with
      | .ok result =>
        -- A deadline-cut ladder can return zero measured points (e.g. the
        -- deadline expired between `shouldStart` and the first probe rung).
        -- Demote to skipped so the suite report and JSONL export don't
        -- claim a successful run with no data — Codex flagged this in
        -- review of the issue #9 PR.
        if result.points.isEmpty then
          entries := entries.push {
            function := entry.spec.name
            kindStr := Schema.kindParametric
            budgetStatus := .skipped
            elapsedSeconds := dt
            errorMessage? := some deadlineDuringRunMsg }
        else
          entries := entries.push {
            function := entry.spec.name
            kindStr := Schema.kindParametric
            budgetStatus := .completed
            parametric? := some result
            elapsedSeconds := dt }
      | .error e =>
        -- A misbehaving benchmark (e.g. invalid declared config) shouldn't
        -- collapse the suite — but the exception text is user-facing
        -- diagnostic data, so we record it on the entry rather than
        -- coercing it into a vanilla budget skip.
        entries := entries.push {
          function := entry.spec.name
          kindStr := Schema.kindParametric
          budgetStatus := .skipped
          elapsedSeconds := dt
          errorMessage? := some s!"runBenchmark failed: {e.toString}" }
    else
      entries := entries.push {
        function := entry.spec.name
        kindStr := Schema.kindParametric
        budgetStatus := .skipped
        errorMessage? := some budgetExhaustedMsg }
  for entry in fEntries do
    let now ← IO.monoNanosNow
    if shouldStart cfg deadline now then
      let benchStart ← IO.monoNanosNow
      let resultE ← (runFixedBenchmark entry.spec.name fixedOverride
        (deadline? := some deadline)).toBaseIO
      let benchEnd ← IO.monoNanosNow
      let dt : Float := (benchEnd - benchStart).toFloat / 1.0e9
      match resultE with
      | .ok result =>
        if result.points.isEmpty then
          entries := entries.push {
            function := entry.spec.name
            kindStr := Schema.kindFixed
            budgetStatus := .skipped
            elapsedSeconds := dt
            errorMessage? := some deadlineDuringRunMsg }
        else
          entries := entries.push {
            function := entry.spec.name
            kindStr := Schema.kindFixed
            budgetStatus := .completed
            fixed? := some result
            elapsedSeconds := dt }
      | .error e =>
        entries := entries.push {
          function := entry.spec.name
          kindStr := Schema.kindFixed
          budgetStatus := .skipped
          elapsedSeconds := dt
          errorMessage? := some s!"runFixedBenchmark failed: {e.toString}" }
    else
      entries := entries.push {
        function := entry.spec.name
        kindStr := Schema.kindFixed
        budgetStatus := .skipped
        errorMessage? := some budgetExhaustedMsg }
  let now ← IO.monoNanosNow
  let totalElapsed : Float := (now - t0).toFloat / 1.0e9
  return { budget := cfg, elapsedSeconds := totalElapsed, entries }

/-! ## JSONL exporter

A unified writer for the suite-mode JSONL report. Each line is one
of:

- a parametric-measurement row (mirroring the child's parametric
  emit, with `budget_status: "completed"` added),
- a fixed-measurement row (mirroring the child's fixed emit, with
  `budget_status: "completed"` added),
- a synthetic placeholder row for a skipped benchmark, carrying
  `status: "error"`, an explanatory `error` message, and
  `budget_status: "skipped"`. The placeholder uses zero values for
  the required numeric fields and `null` for `result_hash`; this is
  the most consistent way to keep the row structurally identical to
  a measurement row while still being clearly tagged as not having
  run.

Producers for these row kinds are kept in `Suite.lean` rather than
shared with `Child.lean` because the suite is the only path that
emits `budget_status`. Lifting a few JSON helpers is cheaper than
threading an additional `Option BudgetStatus` argument through the
child's hot single-batch emitter. -/

private def jsonEscape (s : String) : String :=
  s.foldl (init := "") fun acc c =>
    match c with
    | '"' => acc ++ "\\\""
    | '\\' => acc ++ "\\\\"
    | '\n' => acc ++ "\\n"
    | '\r' => acc ++ "\\r"
    | '\t' => acc ++ "\\t"
    | c => acc.push c

private def jsonStr (s : String) : String := s!"\"{jsonEscape s}\""

private def jsonOptStr : Option String → String
  | none => "null"
  | some s => jsonStr s

private def jsonOptHash : Option UInt64 → String
  | none => "null"
  | some h => s!"\"0x{String.ofList (Nat.toDigits 16 h.toNat)}\""

private def statusJson : Status → String
  | .ok => "\"ok\""
  | .timedOut => "\"timed_out\""
  | .killedAtCap => "\"killed_at_cap\""
  | .error _ => "\"error\""

private def errorMsgFor : Status → Option String
  | .error msg => some msg
  | _ => none

/-- Render one parametric measurement row for the suite-export
    stream. -/
def renderParametricRow (function : Lean.Name) (dp : DataPoint)
    (cacheMode : CacheMode) (budget : BudgetStatus) : String :=
  let perCall : Float :=
    if dp.innerRepeats == 0 then 0.0
    else dp.totalNanos.toFloat / dp.innerRepeats.toFloat
  "{" ++ String.intercalate "," [
    s!"\"schema_version\":{Schema.schemaVersion}",
    s!"\"kind\":{jsonStr Schema.kindParametric}",
    s!"\"function\":{jsonStr function.toString}",
    s!"\"param\":{dp.param}",
    s!"\"inner_repeats\":{dp.innerRepeats}",
    s!"\"total_nanos\":{dp.totalNanos}",
    s!"\"per_call_nanos\":{perCall}",
    s!"\"cache_mode\":{jsonStr cacheMode.toJsonString}",
    s!"\"result_hash\":{jsonOptHash dp.resultHash}",
    s!"\"status\":{statusJson dp.status}",
    s!"\"error\":{jsonOptStr (errorMsgFor dp.status)}",
    s!"\"budget_status\":{jsonStr budget.toJsonString}"
  ] ++ "}"

/-- Render one fixed measurement row for the suite-export stream. -/
def renderFixedRow (function : Lean.Name) (dp : FixedDataPoint)
    (budget : BudgetStatus) : String :=
  "{" ++ String.intercalate "," [
    s!"\"schema_version\":{Schema.schemaVersion}",
    s!"\"kind\":{jsonStr Schema.kindFixed}",
    s!"\"function\":{jsonStr function.toString}",
    s!"\"repeat_index\":{dp.repeatIndex}",
    s!"\"total_nanos\":{dp.totalNanos}",
    s!"\"result_hash\":{jsonOptHash dp.resultHash}",
    s!"\"status\":{statusJson dp.status}",
    s!"\"error\":{jsonOptStr (errorMsgFor dp.status)}",
    s!"\"budget_status\":{jsonStr budget.toJsonString}"
  ] ++ "}"

/-- Render one synthetic skip row. `kindStr` is one of
    `Schema.kindParametric` / `Schema.kindFixed`; the appropriate
    kind-specific required fields (`param` / `inner_repeats` for
    parametric, `repeat_index` for fixed) are filled with `0`.

    `errorMessage?` is the precise reason the entry was skipped —
    "skipped: budget exhausted before start" for a true budget skip,
    or the text of an exception thrown by `runBenchmark` for a config
    validation failure or registry bug. Defaulting to the budget
    message when `none` keeps backward compatibility with callers
    that don't yet thread a reason through, but the suite scheduler
    always supplies one. -/
def renderSkipRow (function : Lean.Name) (kindStr : String)
    (errorMessage? : Option String := none) : String :=
  let kindKeys : List String :=
    if kindStr == Schema.kindFixed then
      [s!"\"repeat_index\":0"]
    else
      ["\"param\":0", "\"inner_repeats\":0"]
  let errorMsg : String :=
    errorMessage?.getD "skipped: budget exhausted before start"
  "{" ++ String.intercalate "," (
    [ s!"\"schema_version\":{Schema.schemaVersion}",
      s!"\"kind\":{jsonStr kindStr}",
      s!"\"function\":{jsonStr function.toString}" ] ++
    kindKeys ++
    [ "\"total_nanos\":0",
      "\"result_hash\":null",
      s!"\"status\":{jsonStr Schema.statusError}",
      s!"\"error\":{jsonStr errorMsg}",
      s!"\"budget_status\":{jsonStr Schema.budgetStatusSkipped}" ]
  ) ++ "}"

/-- Serialize a whole `SuiteReport` to the JSONL export format.
    Returns an array of JSON-formatted lines (no trailing newline)
    so callers can write them via `IO.FS.writeFile` after joining
    on `"\n"` or stream them one by one. -/
def renderSuiteJsonl (report : SuiteReport) : Array String := Id.run do
  let mut out : Array String := #[]
  for entry in report.entries do
    match entry.budgetStatus with
    | .skipped =>
      out := out.push (renderSkipRow entry.function entry.kindStr entry.errorMessage?)
    | .completed =>
      match entry.parametric?, entry.fixed? with
      | some r, _ =>
        for dp in r.points do
          out := out.push (renderParametricRow r.function dp r.config.cacheMode .completed)
      | _, some r =>
        for dp in r.points do
          out := out.push (renderFixedRow r.function dp .completed)
      | _, _ => pure ()
  return out

/-- Write a `SuiteReport` to `path` as JSONL (one row per line,
    trailing newline). -/
def writeSuiteJsonl (report : SuiteReport) (path : System.FilePath) : IO Unit := do
  let lines := renderSuiteJsonl report
  let body := String.intercalate "\n" lines.toList ++ "\n"
  IO.FS.writeFile path body

end Suite
end LeanBench
