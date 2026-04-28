import Lean
import LeanBench.Core
import LeanBench.Env
import LeanBench.RunEnv
import LeanBench.Schema
import LeanBench.Format

/-!
# `LeanBench.Export` -- machine-readable export and baseline comparison

Three responsibilities:

1. **Export.** Serialize `BenchmarkResult` and `FixedResult` into a
   single, documented JSON document suitable for saving to disk, feeding
   to CI scripts, or diffing across commits.

2. **Import.** Deserialize the same JSON format back into in-memory
   types so a saved file can be loaded as a baseline.

3. **Baseline comparison.** Compare a freshly-run result against a
   loaded baseline at shared parameter values. Report regressions and
   improvements.

The export format is a single JSON object (not JSONL). The JSONL
wire format (`Schema.lean`, `Child.lean`) is the internal
child-to-parent protocol; this module owns the user-facing file
format. They use separate version counters
(`export_schema_version` vs `schema_version`) so they can evolve
independently -- see `doc/schema.md#export-format`.

Issue #3.
-/

open Lean (Json Name)

namespace LeanBench
namespace Export

/-! ## Export schema version -/

/-- Current export-format version. Separate from `Schema.schemaVersion`
(the JSONL wire format). Bump on breaking changes to the export
structure; additive fields (new optional keys) do not bump. -/
def exportSchemaVersion : Nat := 1

/-- The set of `export_schema_version` values this reader can parse. -/
def supportedExportVersions : Array Nat := #[exportSchemaVersion]

/-- Validate the `export_schema_version` on a loaded document. -/
def checkExportVersion (json : Json) : Except String Unit := do
  match json.getObjValAs? Nat "export_schema_version" with
  | .ok v =>
    if supportedExportVersions.contains v then .ok ()
    else if v > exportSchemaVersion then
      .error s!"export_schema_version {v} is newer than this reader (max {exportSchemaVersion}); upgrade lean-bench"
    else
      .error s!"export_schema_version {v} is not supported (supported: {supportedExportVersions})"
  | .error _ =>
    .error "missing or invalid export_schema_version"

/-! ## JSON helpers

Mirrors `RunEnv.lean`'s private helpers. Duplicated rather than
exposed because the two modules move independently and the helpers
are trivial. -/

private def jStr (s : String) : Json := Json.str s
private def jNat (n : Nat) : Json := Json.num (.fromNat n)
/-- Convert a `Float` to a `Json` number. `Lean.JsonNumber` has no
`fromFloat` constructor; we go through `toString` → `Json.parse`.
The fallback to `0` should never fire on finite floats. -/
private def jFloat (f : Float) : Json :=
  match Lean.Json.parse (toString f) with
  | .ok (Json.num n) => Json.num n
  | _ => Json.num (.fromNat 0)
private def jBool (b : Bool) : Json := Json.bool b

private def jOptStr : Option String -> Json
  | none   => Json.null
  | some s => Json.str s

private def jOptNat : Option Nat -> Json
  | none   => Json.null
  | some n => Json.num (.fromNat n)

private def jOptFloat : Option Float -> Json
  | none   => Json.null
  | some f => jFloat f

private def jOptBool : Option Bool -> Json
  | none   => Json.null
  | some b => Json.bool b

/-- Parse a hex-prefixed UInt64 string like `"0xdeadbeef"`. -/
private def parseHexU64 (s : String) : Option UInt64 := do
  guard (s.startsWith "0x")
  let body := s.drop 2
  let n := body.foldl (init := 0) fun acc c =>
    let d :=
      if c.isDigit then c.toNat - '0'.toNat
      else if 'a'.toNat <= c.toNat && c.toNat <= 'f'.toNat then c.toNat - 'a'.toNat + 10
      else if 'A'.toNat <= c.toNat && c.toNat <= 'F'.toNat then c.toNat - 'A'.toNat + 10
      else 0
    acc * 16 + d
  return n.toUInt64

private def jOptHash : Option UInt64 -> Json
  | none   => Json.null
  | some h => jStr s!"0x{String.ofList (Nat.toDigits 16 h.toNat)}"

/-! ## Serialization: types -> JSON -/

def statusToString : Status -> String
  | .ok          => "ok"
  | .timedOut    => "timed_out"
  | .killedAtCap => "killed_at_cap"
  | .error _     => "error"

def verdictToString : Verdict -> String
  | .consistentWithDeclaredComplexity => "consistent_with_declared_complexity"
  | .inconclusive                     => "inconclusive"

def cacheModeToString : CacheMode -> String
  | .warm => "warm"
  | .cold => "cold"

def advisoryToString : Advisory -> String
  | .belowSignalFloor             => "below_signal_floor"
  | .partiallyBelowSignalFloor .. => "partially_below_signal_floor"
  | .allCapped                    => "all_capped"
  | .truncatedAtCap ..            => "truncated_at_cap"
  | .tooFewVerdictRows ..         => "too_few_verdict_rows"

def paramScheduleToJson : ParamSchedule -> Json
  | .doubling     => jStr "doubling"
  | .linear n     => Json.mkObj [("kind", jStr "linear"), ("samples", jNat n)]
  | .custom ps    => Json.mkObj [("kind", jStr "custom"), ("params", Json.arr (ps.map jNat))]
  | .auto         => jStr "auto"

def dataPointToJson (dp : DataPoint) : Json :=
  Json.mkObj [
    ("param",             jNat dp.param),
    ("per_call_nanos",    jFloat dp.perCallNanos),
    ("total_nanos",       jNat dp.totalNanos),
    ("inner_repeats",     jNat dp.innerRepeats),
    ("status",            jStr (statusToString dp.status)),
    ("result_hash",       jOptHash dp.resultHash),
    ("trial_index",       jNat dp.trialIndex),
    ("part_of_verdict",   jBool dp.partOfVerdict),
    ("below_signal_floor", jBool dp.belowSignalFloor)
  ]

def fixedDataPointToJson (dp : FixedDataPoint) : Json :=
  Json.mkObj [
    ("repeat_index", jNat dp.repeatIndex),
    ("total_nanos",  jNat dp.totalNanos),
    ("status",       jStr (statusToString dp.status)),
    ("result_hash",  jOptHash dp.resultHash)
  ]

def benchmarkConfigToJson (c : BenchmarkConfig) : Json :=
  Json.mkObj [
    ("max_seconds_per_call",     jFloat c.maxSecondsPerCall),
    ("target_inner_nanos",       jNat c.targetInnerNanos),
    ("param_ceiling",            jNat c.paramCeiling),
    ("param_floor",              jNat c.paramFloor),
    ("verdict_warmup_fraction",  jFloat c.verdictWarmupFraction),
    ("slope_tolerance",          jFloat c.slopeTolerance),
    ("narrow_range_noise_floor", jFloat c.narrowRangeNoiseFloor),
    ("signal_floor_multiplier",  jFloat c.signalFloorMultiplier),
    ("cache_mode",               jStr (cacheModeToString c.cacheMode)),
    ("param_schedule",           paramScheduleToJson c.paramSchedule),
    ("outer_trials",             jNat c.outerTrials)
  ]

def fixedBenchmarkConfigToJson (c : FixedBenchmarkConfig) : Json :=
  Json.mkObj [
    ("repeats",              jNat c.repeats),
    ("max_seconds_per_call", jFloat c.maxSecondsPerCall),
    ("warmup",               jBool c.warmup)
  ]

def trialSummaryToJson (ts : TrialSummary) : Json :=
  Json.mkObj [
    ("param",                  jNat ts.param),
    ("ok_count",               jNat ts.okCount),
    ("median_per_call_nanos",  jFloat ts.medianPerCallNanos),
    ("min_per_call_nanos",     jFloat ts.minPerCallNanos),
    ("max_per_call_nanos",     jFloat ts.maxPerCallNanos),
    ("relative_spread",        jFloat ts.relativeSpread)
  ]

def ratioToJson (r : Ratio) : Json :=
  Json.arr #[jNat r.1, jFloat r.2]

def benchmarkResultToJson (r : BenchmarkResult) : Json :=
  Json.mkObj [
    ("kind",                    jStr "parametric"),
    ("function",                jStr r.function.toString),
    ("complexity_formula",      jStr r.complexityFormula),
    ("hashable",                jBool r.hashable),
    ("config",                  benchmarkConfigToJson r.config),
    ("points",                  Json.arr (r.points.map dataPointToJson)),
    ("ratios",                  Json.arr (r.ratios.map ratioToJson)),
    ("verdict",                 jStr (verdictToString r.verdict)),
    ("c_min",                   jOptFloat r.cMin?),
    ("c_max",                   jOptFloat r.cMax?),
    ("slope",                   jOptFloat r.slope?),
    ("verdict_dropped_leading", jNat r.verdictDroppedLeading),
    ("spawn_floor_nanos",       jOptNat r.spawnFloorNanos?),
    ("advisories",              Json.arr (r.advisories.map (jStr <| advisoryToString ·))),
    ("trial_summaries",         Json.arr (r.trialSummaries.map trialSummaryToJson)),
    ("env",                     match r.env? with
                                | some env => RunEnv.toJson env
                                | none     => Json.null)
  ]

def fixedResultToJson (r : FixedResult) : Json :=
  Json.mkObj [
    ("kind",          jStr "fixed"),
    ("function",      jStr r.function.toString),
    ("hashable",      jBool r.hashable),
    ("config",        fixedBenchmarkConfigToJson r.config),
    ("points",        Json.arr (r.points.map fixedDataPointToJson)),
    ("median_nanos",  jOptNat r.medianNanos?),
    ("min_nanos",     jOptNat r.minNanos?),
    ("max_nanos",     jOptNat r.maxNanos?),
    ("hashes_agree",  jBool r.hashesAgree),
    ("env",           match r.env? with
                      | some env => RunEnv.toJson env
                      | none     => Json.null)
  ]

/-- Build the top-level export JSON document. -/
def toJson
    (parametric : Array BenchmarkResult)
    (fixed : Array FixedResult)
    (env? : Option Env := none) :
    Json :=
  let results : Array Json :=
    (parametric.map benchmarkResultToJson) ++
    (fixed.map fixedResultToJson)
  Json.mkObj [
    ("export_schema_version", jNat exportSchemaVersion),
    ("lean_bench_version",    jStr libraryVersion),
    ("env",                   match env? with
                              | some env => RunEnv.toJson env
                              | none     => Json.null),
    ("results",               Json.arr results)
  ]

/-- Write the export document to a file (pretty-printed). -/
def exportToFile
    (path : System.FilePath)
    (parametric : Array BenchmarkResult)
    (fixed : Array FixedResult)
    (env? : Option Env := none) :
    IO Unit := do
  let json := toJson parametric fixed env?
  IO.FS.writeFile path (json.pretty ++ "\n")

/-! ## Deserialization: JSON -> types -/

private def parseStatus (s : String) : Status :=
  match s with
  | "ok"            => .ok
  | "timed_out"     => .timedOut
  | "killed_at_cap" => .killedAtCap
  | "error"         => .error ""
  | other           => .error s!"unknown status: {other}"

private def parseVerdict (s : String) : Verdict :=
  match s with
  | "consistent_with_declared_complexity" => .consistentWithDeclaredComplexity
  | _                                     => .inconclusive

private def parseCacheMode (s : String) : CacheMode :=
  match s with
  | "cold" => .cold
  | _      => .warm

private def getOptStr (j : Json) (k : String) : Option String :=
  match j.getObjValAs? String k with | .ok s => some s | .error _ => none

private def getOptNat (j : Json) (k : String) : Option Nat :=
  match j.getObjValAs? Nat k with | .ok n => some n | .error _ => none

private def getOptFloat (j : Json) (k : String) : Option Float :=
  match j.getObjValAs? Float k with | .ok f => some f | .error _ => none

private def getOptBool (j : Json) (k : String) : Option Bool :=
  match j.getObjValAs? Bool k with | .ok b => some b | .error _ => none

private def getFloat (j : Json) (k : String) (default : Float := 0.0) : Float :=
  (getOptFloat j k).getD default

private def getNat (j : Json) (k : String) (default : Nat := 0) : Nat :=
  (getOptNat j k).getD default

private def getBool (j : Json) (k : String) (default : Bool := false) : Bool :=
  (getOptBool j k).getD default

private def getStr (j : Json) (k : String) (default : String := "") : String :=
  (getOptStr j k).getD default

def dataPointFromJson (j : Json) : DataPoint :=
  let hashStr := getOptStr j "result_hash"
  let resultHash : Option UInt64 := hashStr.bind parseHexU64
  { param            := getNat j "param"
    perCallNanos     := getFloat j "per_call_nanos"
    totalNanos       := getNat j "total_nanos"
    innerRepeats     := getNat j "inner_repeats"
    status           := parseStatus (getStr j "status")
    resultHash       := resultHash
    trialIndex       := getNat j "trial_index"
    partOfVerdict    := getBool j "part_of_verdict" true
    belowSignalFloor := getBool j "below_signal_floor" }

def fixedDataPointFromJson (j : Json) : FixedDataPoint :=
  let hashStr := getOptStr j "result_hash"
  let resultHash : Option UInt64 := hashStr.bind parseHexU64
  { repeatIndex := getNat j "repeat_index"
    totalNanos  := getNat j "total_nanos"
    status      := parseStatus (getStr j "status")
    resultHash  := resultHash }

private def parseParamSchedule (j : Json) (k : String) : ParamSchedule :=
  match j.getObjVal? k with
  | .ok (.str "doubling") => .doubling
  | .ok (.str "auto") => .auto
  | .ok (.str "linear") => .linear  -- back-compat: old string form
  | .ok obj =>
    let kind := getStr obj "kind" "auto"
    match kind with
    | "linear" => .linear (getNat obj "samples" 16)
    | "custom" =>
      match obj.getObjVal? "params" with
      | .ok (.arr ps) =>
        let params := ps.filterMap fun p =>
          match p with
          | .num n => some n.toFloat.toUInt64.toNat
          | _ => none
        .custom params
      | _ => .auto
    | _ => .auto
  | .error _ => .auto

def benchmarkConfigFromJson (j : Json) : BenchmarkConfig :=
  { maxSecondsPerCall     := getFloat j "max_seconds_per_call" 1.0
    targetInnerNanos      := getNat j "target_inner_nanos" 500_000_000
    paramCeiling          := getNat j "param_ceiling" 1_073_741_824
    paramFloor            := getNat j "param_floor"
    verdictWarmupFraction := getFloat j "verdict_warmup_fraction" 0.2
    slopeTolerance        := getFloat j "slope_tolerance" 0.15
    narrowRangeNoiseFloor := getFloat j "narrow_range_noise_floor" 1.50
    signalFloorMultiplier := getFloat j "signal_floor_multiplier" 10.0
    cacheMode             := parseCacheMode (getStr j "cache_mode" "warm")
    paramSchedule         := parseParamSchedule j "param_schedule"
    outerTrials           := getNat j "outer_trials" 1 }

def fixedBenchmarkConfigFromJson (j : Json) : FixedBenchmarkConfig :=
  { repeats           := getNat j "repeats" 5
    maxSecondsPerCall := getFloat j "max_seconds_per_call" 60.0
    warmup            := getBool j "warmup" true }

def trialSummaryFromJson (j : Json) : TrialSummary :=
  { param              := getNat j "param"
    okCount            := getNat j "ok_count"
    medianPerCallNanos := getFloat j "median_per_call_nanos"
    minPerCallNanos    := getFloat j "min_per_call_nanos"
    maxPerCallNanos    := getFloat j "max_per_call_nanos"
    relativeSpread     := getFloat j "relative_spread" }

def ratioFromJson (j : Json) : Option Ratio :=
  match j with
  | .arr #[p, c] =>
    match p, c with
    | .num pn, .num cn => some (pn.toFloat.toUInt64.toNat, cn.toFloat)
    | _, _ => none
  | _ => none

private def envFromJson? (j : Json) (k : String) : Option Env :=
  match j.getObjVal? k with
  | .ok (.null) => none
  | .ok envJson =>
    match RunEnv.fromJson envJson with
    | .ok env => some env
    | .error _ => none
  | .error _ => none

def benchmarkResultFromJson (j : Json) : Except String BenchmarkResult := do
  let function := (getStr j "function").toName
  let configJson := match j.getObjVal? "config" with
    | .ok c => c | .error _ => Json.null
  let pointsArr := match j.getObjVal? "points" with
    | .ok (.arr ps) => ps | _ => #[]
  let ratiosArr := match j.getObjVal? "ratios" with
    | .ok (.arr rs) => rs | _ => #[]
  let advisoriesArr := match j.getObjVal? "advisories" with
    | .ok (.arr as_) => as_ | _ => #[]
  let trialSummariesArr := match j.getObjVal? "trial_summaries" with
    | .ok (.arr ts) => ts | _ => #[]
  let points := pointsArr.map dataPointFromJson
  let ratios := ratiosArr.filterMap ratioFromJson
  let trialSummaries := trialSummariesArr.map trialSummaryFromJson
  let _ := advisoriesArr  -- advisories are informational, not round-tripped to enum
  return {
    function
    complexityFormula := getStr j "complexity_formula"
    hashable          := getBool j "hashable"
    config            := benchmarkConfigFromJson configJson
    points
    ratios
    verdict           := parseVerdict (getStr j "verdict")
    cMin?             := getOptFloat j "c_min"
    cMax?             := getOptFloat j "c_max"
    slope?            := getOptFloat j "slope"
    verdictDroppedLeading := getNat j "verdict_dropped_leading"
    spawnFloorNanos?  := getOptNat j "spawn_floor_nanos"
    advisories        := #[]   -- not round-tripped; informational
    trialSummaries    := trialSummaries
    env?              := envFromJson? j "env"
  }

def fixedResultFromJson (j : Json) : Except String FixedResult := do
  let function := (getStr j "function").toName
  let configJson := match j.getObjVal? "config" with
    | .ok c => c | .error _ => Json.null
  let pointsArr := match j.getObjVal? "points" with
    | .ok (.arr ps) => ps | _ => #[]
  let points := pointsArr.map fixedDataPointFromJson
  return {
    function
    hashable     := getBool j "hashable"
    config       := fixedBenchmarkConfigFromJson configJson
    points
    medianNanos? := getOptNat j "median_nanos"
    minNanos?    := getOptNat j "min_nanos"
    maxNanos?    := getOptNat j "max_nanos"
    hashesAgree  := getBool j "hashes_agree" true
    env?         := envFromJson? j "env"
  }

/-- Parse the results array from an export document. Returns
(parametric, fixed) results, matched by the `kind` discriminator. -/
private def parseResults (resultsJson : Array Json) :
    Except String (Array BenchmarkResult × Array FixedResult) := do
  let mut parametric : Array BenchmarkResult := #[]
  let mut fixed : Array FixedResult := #[]
  for rj in resultsJson do
    let kind := getStr rj "kind" "parametric"
    match kind with
    | "parametric" =>
      let r ← benchmarkResultFromJson rj
      parametric := parametric.push r
    | "fixed" =>
      let r ← fixedResultFromJson rj
      fixed := fixed.push r
    | other =>
      .error s!"unknown result kind: {other}"
  return (parametric, fixed)

/-- Load an export document from a file path. Returns the parsed
(parametric, fixed) results plus the top-level env if present. -/
def loadBaseline (path : System.FilePath) :
    IO (Array BenchmarkResult × Array FixedResult × Option Env) := do
  let contents ← IO.FS.readFile path
  let json ← IO.ofExcept (Json.parse contents)
  IO.ofExcept (checkExportVersion json)
  let resultsArr := match json.getObjVal? "results" with
    | .ok (.arr rs) => rs | _ => #[]
  let (parametric, fixed) ← IO.ofExcept (parseResults resultsArr)
  let env? := envFromJson? json "env"
  return (parametric, fixed, env?)

/-! ## Baseline comparison -/

/-- Classification of a per-param timing change relative to a baseline. -/
inductive BaselineChange
  | regression (pctChange : Float)
  | improvement (pctChange : Float)
  | stable (pctChange : Float)
  deriving Repr, Inhabited

/-- Per-param comparison between baseline and current. -/
structure ParamComparison where
  param           : Nat
  baselinePerCall : Float
  currentPerCall  : Float
  pctChange       : Float   -- (current - baseline) / baseline * 100
  change          : BaselineChange
  deriving Repr, Inhabited

/-- Baseline comparison summary for one function. -/
structure BaselineReport where
  function         : Name
  kind             : String
  /-- Per-param comparisons, parametric only. -/
  paramComparisons : Array ParamComparison := #[]
  /-- Overall summary for fixed benchmarks. -/
  baselineMedian?  : Option Float := none
  currentMedian?   : Option Float := none
  fixedPctChange?  : Option Float := none
  regressionCount  : Nat := 0
  improvementCount : Nat := 0
  deriving Repr, Inhabited

/-- Compute percentage change and classify it. -/
private def classifyChange (baseline current threshold : Float) : BaselineChange :=
  if baseline <= 0.0 then .stable 0.0
  else
    let pct := (current - baseline) / baseline * 100.0
    if pct > threshold then .regression pct
    else if pct < -threshold then .improvement pct
    else .stable pct

/-- Get the representative per-call nanos for a param from a result.
Uses the trial summary median when available (outerTrials > 1),
otherwise falls back to the first ok data point. -/
private def perCallAtParam (r : BenchmarkResult) (param : Nat) : Option Float :=
  -- Try trial summaries first (outerTrials > 1)
  match r.trialSummaries.find? (·.param == param) with
  | some ts => some ts.medianPerCallNanos
  | none =>
    -- Fall back to single data point
    r.points.findSome? fun dp =>
      if dp.param == param && dp.status == .ok && dp.partOfVerdict
          && !dp.belowSignalFloor then
        some dp.perCallNanos
      else none

/-- Compare a parametric result against its baseline at shared params. -/
def compareParametric
    (baseline current : BenchmarkResult)
    (threshold : Float := 10.0) :
    BaselineReport := Id.run do
  -- Find shared params (verdict-eligible ok params in both)
  let baseParams : Std.HashSet Nat :=
    baseline.points.foldl (init := {}) fun s dp =>
      if dp.status == .ok && dp.partOfVerdict && !dp.belowSignalFloor then
        s.insert dp.param
      else s
  let mut comparisons : Array ParamComparison := #[]
  let mut regressions : Nat := 0
  let mut improvements : Nat := 0
  -- Walk current's params in order
  let mut seen : Std.HashSet Nat := {}
  for dp in current.points do
    if dp.status != .ok || !dp.partOfVerdict || dp.belowSignalFloor then continue
    if seen.contains dp.param then continue
    seen := seen.insert dp.param
    unless baseParams.contains dp.param do continue
    let some bpc := perCallAtParam baseline dp.param | continue
    let some cpc := perCallAtParam current dp.param | continue
    let change := classifyChange bpc cpc threshold
    let pct := if bpc <= 0.0 then 0.0
               else (cpc - bpc) / bpc * 100.0
    comparisons := comparisons.push {
      param := dp.param, baselinePerCall := bpc,
      currentPerCall := cpc, pctChange := pct, change }
    match change with
    | .regression _  => regressions := regressions + 1
    | .improvement _ => improvements := improvements + 1
    | .stable _      => pure ()
  return {
    function := current.function
    kind := "parametric"
    paramComparisons := comparisons
    regressionCount := regressions
    improvementCount := improvements }

/-- Compare a fixed result against its baseline. -/
def compareFixed
    (baseline current : FixedResult)
    (threshold : Float := 10.0) :
    BaselineReport := Id.run do
  match baseline.medianNanos?, current.medianNanos? with
  | some bNanos, some cNanos =>
    let bMed := bNanos.toFloat
    let cMed := cNanos.toFloat
    let change := classifyChange bMed cMed threshold
    let pct := if bMed <= 0.0 then 0.0
               else (cMed - bMed) / bMed * 100.0
    let (reg, imp) := match change with
      | .regression _  => (1, 0)
      | .improvement _ => (0, 1)
      | .stable _      => (0, 0)
    return {
      function := current.function
      kind := "fixed"
      baselineMedian? := some bMed
      currentMedian? := some cMed
      fixedPctChange? := some pct
      regressionCount := reg
      improvementCount := imp }
  | _, _ =>
    -- One or both results have no successful repeats
    return {
      function := current.function
      kind := "fixed" }

/-- Run baseline comparison across all results. Matches by function
name; unmatched functions are skipped. -/
def runBaseline
    (baseParametric : Array BenchmarkResult)
    (baseFixed : Array FixedResult)
    (curParametric : Array BenchmarkResult)
    (curFixed : Array FixedResult)
    (threshold : Float := 10.0) :
    Array BaselineReport := Id.run do
  let mut reports : Array BaselineReport := #[]
  -- Match parametric by function name
  for cur in curParametric do
    match baseParametric.find? (·.function == cur.function) with
    | some base => reports := reports.push (compareParametric base cur threshold)
    | none => pure ()
  -- Match fixed by function name
  for cur in curFixed do
    match baseFixed.find? (·.function == cur.function) with
    | some base => reports := reports.push (compareFixed base cur threshold)
    | none => pure ()
  return reports

/-! ## Baseline report formatting -/

/-- Format a float as a signed percentage string: `+12.3%` or `-5.1%`. -/
private def fmtPct (pct : Float) : String :=
  let sign := if pct >= 0.0 then "+" else ""
  let mag := pct.abs
  let scaled := (mag * 10.0).round.toUInt64.toNat
  let whole := scaled / 10
  let frac := scaled % 10
  s!"{sign}{if pct < 0.0 then "-" else ""}{whole}.{frac}%"

/-- Pad a string on the left with spaces to width `w`. -/
private def leftpad (s : String) (w : Nat) : String :=
  let pad := w - s.length
  if pad > 0 then String.ofList (List.replicate pad ' ') ++ s else s

/-- Pad a string on the right with spaces to width `w`. -/
private def rightpad (s : String) (w : Nat) : String :=
  let pad := w - s.length
  if pad > 0 then s ++ String.ofList (List.replicate pad ' ') else s

/-- Format one `BaselineReport` as a multi-line block. -/
def fmtBaselineReport (r : BaselineReport) : String := Id.run do
  let mut lines : Array String := #[]
  match r.kind with
  | "parametric" =>
    lines := lines.push s!"{r.function}    [parametric baseline comparison]"
    if r.paramComparisons.isEmpty then
      lines := lines.push "  (no shared params for comparison)"
    else
      -- Build table
      let paramStrs := r.paramComparisons.map (fun c => toString c.param)
      let baseStrs := r.paramComparisons.map (fun c => Format.fmtNanosStr c.baselinePerCall.toUInt64.toNat)
      let curStrs := r.paramComparisons.map (fun c => Format.fmtNanosStr c.currentPerCall.toUInt64.toNat)
      let pctStrs := r.paramComparisons.map (fun c => fmtPct c.pctChange)
      let tagStrs := r.paramComparisons.map fun c =>
        match c.change with
        | .regression _  => "[REGRESSION]"
        | .improvement _ => "[improvement]"
        | .stable _      => ""
      let wParam := paramStrs.foldl (fun m s => max m s.length) "param".length
      let wBase  := baseStrs.foldl (fun m s => max m s.length) "baseline".length
      let wCur   := curStrs.foldl (fun m s => max m s.length) "current".length
      let wPct   := pctStrs.foldl (fun m s => max m s.length) "change".length
      lines := lines.push <|
        "  " ++ leftpad "param" wParam ++
        "  " ++ rightpad "baseline" wBase ++
        "  " ++ rightpad "current" wCur ++
        "  " ++ rightpad "change" wPct ++
        "  status"
      for i in [0 : r.paramComparisons.size] do
        lines := lines.push <|
          "  " ++ leftpad paramStrs[i]! wParam ++
          "  " ++ rightpad baseStrs[i]! wBase ++
          "  " ++ rightpad curStrs[i]! wCur ++
          "  " ++ rightpad pctStrs[i]! wPct ++
          "  " ++ tagStrs[i]!
  | "fixed" =>
    lines := lines.push s!"{r.function}    [fixed baseline comparison]"
    match r.baselineMedian?, r.currentMedian?, r.fixedPctChange? with
    | some bm, some cm, some pct =>
      lines := lines.push <|
        s!"  baseline: {Format.fmtNanosStr bm.toUInt64.toNat}" ++
        s!"    current: {Format.fmtNanosStr cm.toUInt64.toNat}" ++
        s!"    change: {fmtPct pct}"
      if r.regressionCount > 0 then
        lines := lines.push "  [REGRESSION]"
      else if r.improvementCount > 0 then
        lines := lines.push "  [improvement]"
    | _, _, _ =>
      lines := lines.push "  (no successful repeats to compare)"
  | _ => pure ()
  return "\n".intercalate lines.toList

/-- Format a summary of all baseline reports. -/
def fmtBaselineReports (reports : Array BaselineReport) : String := Id.run do
  let mut lines : Array String := #[]
  lines := lines.push s!"baseline comparison ({reports.size} function(s) matched):"
  for r in reports do
    lines := lines.push (fmtBaselineReport r)
    lines := lines.push ""
  let totalReg : Nat := reports.foldl (fun acc r => acc + r.regressionCount) 0
  let totalImp : Nat := reports.foldl (fun acc r => acc + r.improvementCount) 0
  if totalReg > 0 then
    lines := lines.push s!"REGRESSIONS DETECTED: {totalReg} param(s) regressed"
  if totalImp > 0 then
    lines := lines.push s!"improvements: {totalImp} param(s) improved"
  if totalReg == 0 && totalImp == 0 then
    lines := lines.push "no regressions or improvements detected"
  return "\n".intercalate lines.toList

/-- JSON serialization of baseline comparison reports, for embedding
in the export document when both `--export` and `--baseline` are used. -/
def baselineReportToJson (r : BaselineReport) : Json :=
  let comparisons := r.paramComparisons.map fun c =>
    Json.mkObj [
      ("param",           jNat c.param),
      ("baseline_nanos",  jFloat c.baselinePerCall),
      ("current_nanos",   jFloat c.currentPerCall),
      ("pct_change",      jFloat c.pctChange),
      ("status",          jStr (match c.change with
        | .regression _  => "regression"
        | .improvement _ => "improvement"
        | .stable _      => "stable"))
    ]
  Json.mkObj [
    ("function",          jStr r.function.toString),
    ("kind",              jStr r.kind),
    ("comparisons",       Json.arr comparisons),
    ("baseline_median",   match r.baselineMedian? with
                          | some f => jFloat f | none => Json.null),
    ("current_median",    match r.currentMedian? with
                          | some f => jFloat f | none => Json.null),
    ("fixed_pct_change",  match r.fixedPctChange? with
                          | some f => jFloat f | none => Json.null),
    ("regression_count",  jNat r.regressionCount),
    ("improvement_count", jNat r.improvementCount)
  ]

/-- Build a full export document with optional baseline comparison. -/
def toJsonWithBaseline
    (parametric : Array BenchmarkResult)
    (fixed : Array FixedResult)
    (env? : Option Env := none)
    (baseline? : Option (Array BaselineReport) := none) :
    Json :=
  let results : Array Json :=
    (parametric.map benchmarkResultToJson) ++
    (fixed.map fixedResultToJson)
  let comp := match baseline? with
    | some reports => Json.arr (reports.map baselineReportToJson)
    | none => Json.null
  let fields : List (String × Json) := [
    ("export_schema_version", jNat exportSchemaVersion),
    ("lean_bench_version",    jStr libraryVersion),
    ("env",                   match env? with
                              | some env => RunEnv.toJson env
                              | none     => Json.null),
    ("results",               Json.arr results)
  ]
  let allFields := match baseline? with
    | some _ => fields ++ [("baseline_comparison", comp)]
    | none   => fields
  Json.mkObj allFields

end Export
end LeanBench
