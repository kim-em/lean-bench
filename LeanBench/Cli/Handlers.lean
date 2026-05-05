import Cli
import Lean.Data.Json
import LeanBench.Core
import LeanBench.Env
import LeanBench.Run
import LeanBench.Child
import LeanBench.Compare
import LeanBench.Verify
import LeanBench.Format
import LeanBench.Export
import LeanBench.Profile
import LeanBench.Cli.Parse

/-!
# `LeanBench.Cli.Handlers` — subcommand handler implementations

One `runFooCmd : Cli.Parsed → IO UInt32` per subcommand exposed by the
top-level Cmd tree (`list`, `run`, `compare`, `verify`, `profile`,
`_child`). The Cmd tree itself, plus `dispatch`, lives in
[`LeanBench.Cli`](../Cli.lean); the parser plumbing
(`parsedFlag?`, override builders, filter helpers, `BudgetSkip` /
`BudgetSummary`) lives in [`LeanBench.Cli.Parse`](Parse.lean).
-/

open Cli

namespace LeanBench
namespace Cli

def runListCmd (p : Cli.Parsed) : IO UInt32 := do
  let (tagFilter, nameFilter) := parseFilters p
  let pEntries ← allRuntimeEntries
  let fEntries ← allFixedRuntimeEntries
  let pFiltered := filterParametric pEntries tagFilter nameFilter
  let fFiltered := filterFixed fEntries tagFilter nameFilter
  if pFiltered.isEmpty && fFiltered.isEmpty then
    if hasFilters tagFilter nameFilter then
      IO.println "(no benchmarks match the given filters)"
    else
      IO.println "(no benchmarks registered)"
    return 0
  IO.println "registered benchmarks:"
  for e in pFiltered do
    IO.println (Format.fmtSpec e.spec)
  for e in fFiltered do
    IO.println (Format.fmtFixedSpec e.spec)
  return 0

/-- Names of parametric benchmarks in `parametric` that produced no
    verdict-eligible rows. Used to emit a single-line stderr summary
    so CI logs surface the calibration failure without having to
    parse the per-result advisories. Issue #47. -/
private def noUsableDataNames (parametric : Array BenchmarkResult) :
    Array Lean.Name :=
  parametric.filterMap fun r => if r.noUsableData then some r.function else none

/-- Fixed benchmarks whose `expectedHash` check failed. A hash
mismatch is a hard correctness failure, distinct from the soft
`hashesAgree := false` non-determinism warning. Issue #55. -/
private def expectedHashFailureNames (fixed : Array FixedResult) :
    Array Lean.Name :=
  fixed.filterMap fun r =>
    if r.expectedHashCheck.passed then none else some r.function

/-- Shared post-run logic: baseline comparison + export. Returns
the exit code: `exitNoUsableData` (2) if any parametric run produced
zero verdict-eligible rows, else `1` on baseline regressions, else `0`.
Issue #3, issue #47. -/
private def handleBaselineAndExport
    (parametric : Array BenchmarkResult)
    (fixed : Array FixedResult)
    (env? : Option Env)
    (baselinePath? exportPath? : Option String)
    (threshold : Float)
    (budget? : Option BudgetSummary := none) : IO UInt32 := do
  let mut exitCode : UInt32 := 0
  let mut baselineReports? : Option (Array Export.BaselineReport) := none
  match baselinePath? with
  | some bp =>
    let (baseP, baseF, _) ← Export.loadBaseline bp
    let reports := Export.runBaseline baseP baseF parametric fixed threshold
    IO.println ""
    IO.println (Export.fmtBaselineReports reports)
    let totalReg : Nat := reports.foldl (fun acc r => acc + r.regressionCount) 0
    if totalReg > 0 then exitCode := 1
    baselineReports? := some reports
  | none => pure ()
  match exportPath? with
  | some ep =>
    let budgetJson? := budget?.map fun s =>
      Export.budgetSummaryToJson s.totalSeconds s.elapsedSeconds
        s.completed s.truncated
        (s.skipped.map fun sk => (sk.function, sk.kind))
    let json := Export.toJsonWithBaseline parametric fixed env?
      baselineReports? budgetJson?
    IO.FS.writeFile ep (json.pretty ++ "\n")
    IO.println s!"exported to {ep}"
  | none => pure ()
  -- expectedHash mismatch is on equal footing with a baseline
  -- regression — bump to `1` if not already higher. Issue #55.
  let hashFail := expectedHashFailureNames fixed
  unless hashFail.isEmpty do
    let names := String.intercalate ", " (hashFail.toList.map toString)
    IO.eprintln s!"run: {hashFail.size} fixed benchmark(s) failed expectedHash check: {names}"
    if exitCode == 0 then exitCode := 1
  -- exitNoUsableData (2) supersedes (1): with no data the regression
  -- check is meaningless.
  let noData := noUsableDataNames parametric
  unless noData.isEmpty do
    let names := String.intercalate ", " (noData.toList.map toString)
    IO.eprintln s!"run: {noData.size} benchmark(s) produced zero verdict-eligible rows: {names}"
    exitCode := exitNoUsableData
  return exitCode

/-- Dispatch `run` by explicit names and/or filters. When explicit
    names are given, run those directly (existing behavior). When
    only filters are given, run all matching benchmarks. Parametric
    and fixed benchmarks can coexist in a filtered run; each is
    dispatched to its own runner.

    With both names and filters, only explicit names are run (filters
    are selection tools for "find all matching" mode, not additional
    constraints on named benchmarks). -/
def runRunCmd (p : Cli.Parsed) : IO UInt32 := do
  let nameStrs := p.variableArgsAs! String |>.toList
  let (tagFilter, nameFilter) := parseFilters p
  let exportPath? := parsedFlag? p "export-file" String
  let baselinePath? := parsedFlag? p "baseline" String
  let threshold : Float :=
    (parsedFlag? p "regression-threshold" Float).getD 10.0
  -- `--auto-fit` (issue #8) is purely advisory; the declared-model
  -- verdict is unchanged.
  let autoFit := p.hasFlag "auto-fit"
  let fmtP (r : BenchmarkResult) : String :=
    if autoFit then Format.fmtResultWithAutoFit r else Format.fmtResult r
  if !nameStrs.isEmpty then
    let mut anyFail := false
    let mut pResults : Array BenchmarkResult := #[]
    let mut fResults : Array FixedResult := #[]
    for nameStr in nameStrs do
      let name := nameStr.toName
      match ← findRuntimeEntry name with
      | some _ =>
        let result ← LeanBench.runBenchmark name (configOverrideFromParsed p)
        IO.println (fmtP result)
        pResults := pResults.push result
      | none =>
        match ← findFixedRuntimeEntry name with
        | some _ =>
          let result ← LeanBench.runFixedBenchmark name
                          (fixedConfigOverrideFromParsed p)
          IO.println (Format.fmtFixedResult result)
          fResults := fResults.push result
        | none =>
          IO.eprintln s!"run: unregistered benchmark: {name}"
          anyFail := true
      if nameStrs.length > 1 then
        IO.println ""
    if anyFail then return 1
    let env? := pResults[0]?.bind (·.env?) |>.orElse fun _ => fResults[0]?.bind (·.env?)
    let exportCode ← handleBaselineAndExport pResults fResults env?
      baselinePath? exportPath? threshold
    return exportCode
  unless hasFilters tagFilter nameFilter do
    IO.eprintln "run: specify a benchmark name or use --tag/--filter to select benchmarks"
    return 1
  let pEntries ← allRuntimeEntries
  let fEntries ← allFixedRuntimeEntries
  let pFiltered := filterParametric pEntries tagFilter nameFilter
  let fFiltered := filterFixed fEntries tagFilter nameFilter
  if pFiltered.isEmpty && fFiltered.isEmpty then
    IO.eprintln "run: no benchmarks match the given filters"
    return 1
  -- CI-budget mode (issue #9): consult the deadline before each
  -- benchmark and inside `runBenchmark` between rungs.
  let totalSecs? : Option Float := parsedFlag? p "total-seconds" Float
  match totalSecs? with
  | some s =>
    if s.isNaN || s < 0.0 then
      IO.eprintln s!"run: --total-seconds must be ≥ 0; got {s}"
      return 1
  | none => pure ()
  let startMono ← IO.monoNanosNow
  let deadline? : Option Nat := totalSecs?.map fun s =>
    -- A `0` budget marks every benchmark as a budget skip on the
    -- first deadline check.
    startMono + (s * 1.0e9).toUInt64.toNat
  let mut first := true
  let mut pResults : Array BenchmarkResult := #[]
  let mut fResults : Array FixedResult := #[]
  let mut skipped : Array BudgetSkip := #[]
  let mut truncated : Nat := 0
  let mut budgetExhausted := false
  for e in pFiltered do
    if budgetExhausted then
      skipped := skipped.push { function := e.spec.name, kind := "parametric" }
    else if (← LeanBench.deadlineExceeded? deadline?) then
      budgetExhausted := true
      skipped := skipped.push { function := e.spec.name, kind := "parametric" }
    else
      unless first do IO.println ""
      first := false
      let result ← LeanBench.runBenchmark e.spec.name
                      (configOverrideFromParsed p) deadline?
      IO.println (fmtP result)
      if result.budgetTruncated then truncated := truncated + 1
      pResults := pResults.push result
  for e in fFiltered do
    if budgetExhausted then
      skipped := skipped.push { function := e.spec.name, kind := "fixed" }
    else if (← LeanBench.deadlineExceeded? deadline?) then
      budgetExhausted := true
      skipped := skipped.push { function := e.spec.name, kind := "fixed" }
    else
      unless first do IO.println ""
      first := false
      let result ← LeanBench.runFixedBenchmark e.spec.name
                      (fixedConfigOverrideFromParsed p) deadline?
      IO.println (Format.fmtFixedResult result)
      if result.budgetTruncated then truncated := truncated + 1
      fResults := fResults.push result
  let endMono ← IO.monoNanosNow
  let env? := pResults[0]?.bind (·.env?) |>.orElse fun _ => fResults[0]?.bind (·.env?)
  let budget? : Option BudgetSummary := totalSecs?.map fun ts =>
    { totalSeconds   := ts
      elapsedSeconds := (endMono - startMono).toFloat / 1.0e9
      completed      := pResults.size + fResults.size
      skipped
      truncated }
  match budget? with
  | some b =>
    IO.println ""
    IO.println (Format.fmtBudgetSummary b.totalSeconds b.elapsedSeconds
      b.completed b.truncated (b.skipped.map fun s => (s.function, s.kind)))
  | none => pure ()
  handleBaselineAndExport pResults fResults env?
    baselinePath? exportPath? threshold budget?

/-- Dispatch `compare A B …` by registration kind. All listed names
    must be the same kind (all parametric or all fixed); mixing is
    rejected with a user-facing error.

    When no explicit names are given, `--tag` / `--filter` selects
    the benchmarks to compare. The same kind constraint applies. -/
def runCompareCmd (p : Cli.Parsed) : IO UInt32 := do
  let nameStrs := p.variableArgsAs! String |>.toList
  let (tagFilter, nameFilter) := parseFilters p
  let resolved : List Lean.Name ←
    if !nameStrs.isEmpty then
      pure (nameStrs.map String.toName)
    else if hasFilters tagFilter nameFilter then do
      let pEntries ← allRuntimeEntries
      let fEntries ← allFixedRuntimeEntries
      let pFiltered := filterParametric pEntries tagFilter nameFilter
      let fFiltered := filterFixed fEntries tagFilter nameFilter
      let pNames := pFiltered.map (·.spec.name) |>.toList
      let fNames := fFiltered.map (·.spec.name) |>.toList
      pure (pNames ++ fNames)
    else pure []
  if resolved.isEmpty then
    IO.eprintln "compare: need at least two benchmark names (or use --tag/--filter)"
    return 1
  if resolved.length < 2 then
    IO.eprintln "compare: need at least two benchmarks to compare"
    return 1
  let exportPath? : Option String := parsedFlag? p "export-file" String
  let mut allParametric := true
  let mut allFixed := true
  for n in resolved do
    let isP := (← findRuntimeEntry n).isSome
    let isF := (← findFixedRuntimeEntry n).isSome
    unless isP do allParametric := false
    unless isF do allFixed := false
  if allParametric then
    let report ← LeanBench.compare resolved (configOverrideFromParsed p)
    if p.hasFlag "auto-fit" then
      IO.println (Format.fmtComparisonWithAutoFit report)
    else
      IO.println (Format.fmtComparison report)
    match exportPath? with
    | some ep =>
      let env? := report.results[0]?.bind (·.env?)
      Export.exportToFile ep report.results #[] env?
      IO.println s!"exported to {ep}"
    | none => pure ()
    let noData := noUsableDataNames report.results
    unless noData.isEmpty do
      let names := String.intercalate ", " (noData.toList.map toString)
      IO.eprintln s!"compare: {noData.size} benchmark(s) produced zero verdict-eligible rows: {names}"
      return exitNoUsableData
    return 0
  if allFixed then
    let report ← LeanBench.compareFixed resolved
                    (fixedConfigOverrideFromParsed p)
    IO.println (Format.fmtFixedComparison report)
    match exportPath? with
    | some ep =>
      let env? := report.results[0]?.bind (·.env?)
      Export.exportToFile ep #[] report.results env?
      IO.println s!"exported to {ep}"
    | none => pure ()
    -- expectedHash mismatch in the compared set is a hard failure.
    -- Issue #55.
    let hashFail := expectedHashFailureNames report.results
    unless hashFail.isEmpty do
      let names := String.intercalate ", " (hashFail.toList.map toString)
      IO.eprintln s!"compare: {hashFail.size} fixed benchmark(s) failed expectedHash check: {names}"
      return 1
    return 0
  IO.eprintln "compare: cannot mix parametric and fixed benchmarks; or one or more names are unregistered"
  return 1

/-- Dispatch `profile NAME --profiler "perf stat --"`: re-invokes
    this same binary in `_child` mode at a single param, but with the
    user's profiler command in front. See `LeanBench.Profile.runProfile`
    for the full contract. -/
def runProfileCmd (p : Cli.Parsed) : IO UInt32 := do
  let nameStr := (p.positionalArg! "name").as! String
  let name := nameStr.toName
  let profilerCmd := (p.flag! "profiler").as! String
  let param : Nat := (parsedFlag? p "param" Nat).getD 0
  if (LeanBench.profileTokens profilerCmd).isEmpty then
    IO.eprintln "profile: --profiler must be a non-empty command (e.g. \"perf stat --\")"
    return 1
  match ← findRuntimeEntry name with
  | none =>
    -- Fixed-benchmark profiling is not wired up yet — issue #13.
    match ← findFixedRuntimeEntry name with
    | some _ =>
      IO.eprintln s!"profile: {name} is a fixed benchmark; profiling for fixed benchmarks is not yet supported (issue #13)"
      return 1
    | none =>
      IO.eprintln s!"profile: unregistered benchmark: {name}"
      return 1
  | some _ =>
    LeanBench.runProfile name param profilerCmd (configOverrideFromParsed p)

def runVerifyCmd (p : Cli.Parsed) : IO UInt32 := do
  let nameStrs := p.variableArgsAs! String |>.toList
  let (tagFilter, nameFilter) := parseFilters p
  let names : List Lean.Name ←
    if !nameStrs.isEmpty then
      pure (nameStrs.map String.toName)
    else if hasFilters tagFilter nameFilter then do
      let pEntries ← allRuntimeEntries
      let fEntries ← allFixedRuntimeEntries
      let pFiltered := filterParametric pEntries tagFilter nameFilter
      let fFiltered := filterFixed fEntries tagFilter nameFilter
      let pNames := pFiltered.map (·.spec.name) |>.toList
      let fNames := fFiltered.map (·.spec.name) |>.toList
      pure (pNames ++ fNames)
    else pure []
  -- `verify []` means "all"; guard against `--tag x` matching nothing
  -- (which would otherwise silently verify everything).
  if hasFilters tagFilter nameFilter && names.isEmpty then
    IO.eprintln "verify: no benchmarks match the given filters"
    return 1
  match ← (LeanBench.verify names |>.toBaseIO) with
  | .error e =>
    IO.eprintln s!"verify: {e.toString}"
    return (1 : UInt32)
  | .ok reports =>
    IO.println (Format.fmtCombinedVerify reports)
    return if reports.passed then (0 : UInt32) else (1 : UInt32)

/-! ## Subcommand handler (child side) -/

def runChildCmd (p : Cli.Parsed) : IO UInt32 := do
  let env? : Option LeanBench.Env :=
    match parsedFlag? p "env-json" String with
    | none => none
    | some s =>
      match Lean.Json.parse s with
      | .error _ => none
      | .ok j =>
        match LeanBench.RunEnv.fromJson j with
        | .ok env => some env
        | .error _ => none
  let benchStr := (p.flag! "bench").as! String
  if p.hasFlag "fixed" then
    let repeatIdx : Nat :=
      match parsedFlag? p "repeat-index" Nat with
      | some n => n
      | none   => 0
    let minTotalNanos : Nat :=
      (parsedFlag? p "min-total-nanos" Nat).getD 1_000_000
    LeanBench.runFixedChildMode benchStr.toName repeatIdx minTotalNanos env?
  else
    let param := (p.flag! "param").as! Nat
    let targetNanos := (p.flag! "target-nanos").as! Nat
    let cacheMode : CacheMode :=
      (parsedFlag? p "cache-mode" LeanBench.CacheMode).getD .warm
    LeanBench.runChildMode benchStr.toName param targetNanos cacheMode env?

def runProbeFloorCmd (p : Cli.Parsed) : IO UInt32 := do
  let env? : Option LeanBench.Env :=
    match parsedFlag? p "env-json" String with
    | none => none
    | some s =>
      match Lean.Json.parse s with
      | .error _ => none
      | .ok j =>
        match LeanBench.RunEnv.fromJson j with
        | .ok env => some env
        | .error _ => none
  LeanBench.runProbeFloorMode env?

end Cli
end LeanBench
