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

/-!
# `LeanBench.Cli` — argv dispatcher

The user's `main` is just `LeanBench.Cli.dispatch`. The same compiled
binary serves as both parent (orchestrator) and child (single-batch
runner). Subcommand layout:

| invocation                                                      | role |
|-----------------------------------------------------------------|------|
| `./bench`                                                       | parent: print all registered benchmark names |
| `./bench list`                                                  | parent: same |
| `./bench run NAME [override flags]`                             | parent: run one or more benchmarks, print report |
| `./bench compare A B C [override flags]`                        | parent: comparison report |
| `./bench verify [NAMES…]`                                        | parent: bounded `f 0` / `f 1` sanity check via children |
| `./bench profile NAME --profiler "STR" [--param N]`              | parent: re-spawn child wrapped under user's profiler (issue #13) |
| `./bench _child --bench NAME --param N --target-nanos T`        | child: one inner-tuned batch, print one JSONL row, exit |

`run` and `compare` accept run-time `BenchmarkConfig` overrides via
flags (`--max-seconds-per-call`, `--target-inner-nanos`,
`--param-floor`, `--param-ceiling`, `--warmup-fraction`,
`--slope-tolerance`, `--param-schedule`). Each flag corresponds 1:1
with a `ConfigOverride` field; missing flags leave the declared
config untouched.

All parent-side subcommands accept `--tag TAG` and `--filter PATTERN`
for selecting benchmarks by tag or name substring. Tags are declared
via `where { tags := #["sort", "fast"] }` and matched by
comma-separated OR logic (`--tag sort,fast` matches benchmarks with
either tag). `--filter Sort` matches benchmarks whose name contains
"Sort" as a substring. See `doc/quickstart.md#tags-and-filtering`.

CLI parsing uses `Cli` (mhuisi/lean4-cli).
-/

open Cli

namespace LeanBench

/-- A `Float` `ParseableType` for `Cli`. The library ships with `Nat` /
`Int` / `String` / `Bool` instances but not `Float`; we need it for the
fractional `--max-seconds-per-call`, `--warmup-fraction`, and
`--slope-tolerance` overrides. We parse via `Lean.Json` (which already
handles signs, exponents, and fractions) so users can pass `0.5`,
`1e-3`, `3`, etc. -/
instance : Cli.ParseableType Float where
  name := "Float"
  parse?
    | "" => none
    | s  =>
      match Lean.Json.parse s with
      | Except.ok (Lean.Json.num n) => some n.toFloat
      | _ => none

/-- A `ParamSchedule` `ParseableType` for `Cli`. Recognises the three
ladder shapes by name: `auto` (default — auto-pick from declared
complexity), `doubling`, and `linear`. The CLI form intentionally
doesn't expose the `linear samples` count; pin a different sample
count at declaration time via `where { paramSchedule := .linear 32 }`.
String comparison is case-insensitive so `--param-schedule LINEAR`
works too. -/
instance : Cli.ParseableType LeanBench.ParamSchedule where
  name := "ParamSchedule"
  parse? s :=
    match s.toLower with
    | "auto"     => some .auto
    | "doubling" => some .doubling
    | "linear"   => some .linear
    | _          => none

/-- A `CacheMode` `ParseableType` for `Cli`. Recognises `warm` and
`cold`; case-insensitive so `--cache-mode COLD` works. -/
instance : Cli.ParseableType LeanBench.CacheMode where
  name := "CacheMode"
  parse? s :=
    match s.toLower with
    | "warm" => some .warm
    | "cold" => some .cold
    | _      => none

namespace Cli

/-- Read a flag's typed value from a `Cli.Parsed` if it was passed.
Wraps the `flag? / .as!` boilerplate so each `ConfigOverride` field
is one line. -/
private def parsedFlag? (p : Cli.Parsed) (long : String) (τ : Type)
    [Inhabited τ] [Cli.ParseableType τ] : Option τ :=
  p.flag? long |>.map (·.as! τ)

/-- Build a `ConfigOverride` from whichever override flags were passed
on `run` / `compare`. The set of flags is intentionally narrower than
`ConfigOverride` itself — `killGraceMs` is a SIGTERM-vs-SIGKILL
implementation detail, not a benchmark-shape knob, so it stays
declaration-time-only. -/
def configOverrideFromParsed (p : Cli.Parsed) : ConfigOverride :=
  { targetInnerNanos?      := parsedFlag? p "target-inner-nanos" Nat
    maxSecondsPerCall?     := parsedFlag? p "max-seconds-per-call" Float
    paramCeiling?          := parsedFlag? p "param-ceiling" Nat
    paramFloor?            := parsedFlag? p "param-floor" Nat
    verdictWarmupFraction? := parsedFlag? p "warmup-fraction" Float
    slopeTolerance?        := parsedFlag? p "slope-tolerance" Float
    paramSchedule?         := parsedFlag? p "param-schedule" LeanBench.ParamSchedule
    cacheMode?             := parsedFlag? p "cache-mode" LeanBench.CacheMode
    outerTrials?           := parsedFlag? p "outer-trials" Nat }

/-- Build a `FixedConfigOverride` from override flags. Only fields
that share a flag namespace with parametric (`max-seconds-per-call`)
plus the fixed-only `repeats` flag. Other parametric flags
(param-ceiling, slope-tolerance) are ignored; that's deliberate
since they have no meaning for a fixed benchmark. -/
def fixedConfigOverrideFromParsed (p : Cli.Parsed) : FixedConfigOverride :=
  { repeats?           := parsedFlag? p "repeats" Nat
    maxSecondsPerCall? := parsedFlag? p "max-seconds-per-call" Float }

/-! ## Benchmark filtering (issue #10)

Shared filtering infrastructure for `list`, `run`, `compare`, and
`verify`. Benchmarks can be selected by:

- **Tag** (`--tag sort,fast`): matches benchmarks that have at least
  one of the listed tags (OR logic).
- **Name filter** (`--filter Sort`): matches benchmarks whose full
  dotted name contains the given substring.
- **Both** (`--tag sort --filter Insert`): both conditions must match
  (AND logic between the two filter types).

When neither `--tag` nor `--filter` is given, all benchmarks pass the
filter (existing behavior). -/

/-- True iff `haystack` contains `needle` as a substring. -/
def containsSub (haystack needle : String) : Bool :=
  (haystack.splitOn needle).length > 1

/-- Split a comma-separated tag string into individual tags. Trims
whitespace from each segment and filters out empty segments, so
`--tag "sort, fast"` and `--tag sort,,fast` both produce
`#["sort", "fast"]`. -/
def splitTags (s : String) : Array String :=
  (s.splitOn ",").toArray.map (·.trimAscii.toString) |>.filter (!·.isEmpty)

/-- Does a single benchmark entry match the given tag and name
filters? -/
def matchEntry (name : Lean.Name) (entryTags : Array String)
    (tagFilter : Array String) (nameFilter : Option String) : Bool :=
  let tagOk := tagFilter.isEmpty || tagFilter.any entryTags.contains
  let nameOk := match nameFilter with
    | none => true
    | some pat => containsSub (name.toString (escape := false)) pat
  tagOk && nameOk

/-- Parse `--tag` and `--filter` from a `Cli.Parsed`. Returns
`(tagFilter, nameFilter)` where `tagFilter` is the split comma-
separated tag list (empty if `--tag` was not passed) and `nameFilter`
is the raw `--filter` string if present. -/
private def parseFilters (p : Cli.Parsed) : Array String × Option String :=
  let tagFilter := parsedFlag? p "tag" String |>.map splitTags |>.getD #[]
  let nameFilter := parsedFlag? p "filter" String
  (tagFilter, nameFilter)

/-- Filter parametric runtime entries by tag/name. -/
private def filterParametric (entries : Array RuntimeEntry)
    (tagFilter : Array String) (nameFilter : Option String) :
    Array RuntimeEntry :=
  entries.filter fun e =>
    matchEntry e.spec.name e.spec.config.tags tagFilter nameFilter

/-- Filter fixed runtime entries by tag/name. -/
private def filterFixed (entries : Array FixedRuntimeEntry)
    (tagFilter : Array String) (nameFilter : Option String) :
    Array FixedRuntimeEntry :=
  entries.filter fun e =>
    matchEntry e.spec.name e.spec.config.tags tagFilter nameFilter

/-- Are any filter flags set? -/
private def hasFilters (tagFilter : Array String) (nameFilter : Option String) :
    Bool :=
  !tagFilter.isEmpty || nameFilter.isSome

/-! ## CI-budget mode (issue #9)

`--total-seconds N` on `run` puts the suite under a wallclock budget.
Benchmarks are scheduled in their natural order; before each one we
check whether the deadline has passed. Benchmarks that don't fit are
recorded as `BudgetSkip` entries — they appear in the terminal output
and the export document so partial-suite runs are explicit rather
than hidden.

The deadline is also plumbed into `runBenchmark` so a single benchmark
can be cut short between rungs (the `BenchmarkResult.budgetTruncated`
flag). The bound on total wall time is therefore approximately
`total_seconds + maxSecondsPerCall` (one rung may be in flight when
the deadline trips). See `doc/quickstart.md#ci-budget-mode`. -/

/-- A benchmark that wasn't run because the CI budget was exhausted
before it could start. -/
structure BudgetSkip where
  function : Lean.Name
  /-- `"parametric"` or `"fixed"` — matches the export discriminator. -/
  kind     : String
  deriving Inhabited, Repr

/-- Suite-level summary of a budgeted run. Carried alongside the
ordinary results so the CLI report and the export document agree. -/
structure BudgetSummary where
  totalSeconds   : Float
  /-- Wallclock seconds elapsed at the moment the orchestrator stopped
      scheduling new benchmarks (either the suite finished or the
      deadline tripped). -/
  elapsedSeconds : Float
  completed      : Nat
  skipped        : Array BudgetSkip
  /-- Number of completed benchmarks whose internal ladder was cut
      short by the deadline (`BenchmarkResult.budgetTruncated`). -/
  truncated      : Nat
  deriving Inhabited, Repr

/-! ## Subcommand handlers (parent side) -/

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

/-- Shared post-run logic: baseline comparison + export. Returns
the exit code (non-zero on regressions). Issue #3. -/
private def handleBaselineAndExport
    (parametric : Array BenchmarkResult)
    (fixed : Array FixedResult)
    (env? : Option Env)
    (baselinePath? exportPath? : Option String)
    (threshold : Float)
    (budget? : Option BudgetSummary := none) : IO UInt32 := do
  -- Baseline comparison
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
  -- Export
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
  -- `--auto-fit` (issue #8) chooses which result formatter to use
  -- for parametric results. The declared-model verdict is
  -- unaffected — auto-fit is purely advisory.
  let autoFit := p.hasFlag "auto-fit"
  let fmtP (r : BenchmarkResult) : String :=
    if autoFit then Format.fmtResultWithAutoFit r else Format.fmtResult r
  -- Explicit names: run each directly (existing behavior).
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
  -- No explicit names: use filters.
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
  -- Issue #9: optional CI-budget mode. When `--total-seconds` is set,
  -- compute an absolute deadline and consult it before each benchmark
  -- (and inside `runBenchmark` between rungs) so the suite stops
  -- within a predictable bound.
  let totalSecs? : Option Float := parsedFlag? p "total-seconds" Float
  match totalSecs? with
  | some s =>
    if s.isNaN || s < 0.0 then
      IO.eprintln s!"run: --total-seconds must be ≥ 0; got {s}"
      return 1
  | none => pure ()
  let startMono ← IO.monoNanosNow
  let deadline? : Option Nat := totalSecs?.map fun s =>
    -- `s ≥ 0` (validated above), so the conversion is well-defined.
    -- A `0` budget produces a deadline equal to `startMono`, which
    -- the first deadline check will see as exhausted, marking every
    -- benchmark as a budget skip.
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
    -- Fixed benchmarks aren't profiled through this entry point yet —
    -- the param-less single-shot case is the easier one to add later.
    -- For now, point the user at the parametric registry explicitly.
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
  -- `verify []` means "all" (existing behavior). `verify --tag x`
  -- that matches nothing still calls `verify []` which verifies all,
  -- so check for that edge case.
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
  let benchStr := (p.flag! "bench").as! String
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
  if p.hasFlag "fixed" then
    let repeatIdx : Nat :=
      match parsedFlag? p "repeat-index" Nat with
      | some n => n
      | none   => 0
    LeanBench.runFixedChildMode benchStr.toName repeatIdx env?
  else
    let param := (p.flag! "param").as! Nat
    let targetNanos := (p.flag! "target-nanos").as! Nat
    let cacheMode : CacheMode :=
      (parsedFlag? p "cache-mode" LeanBench.CacheMode).getD .warm
    LeanBench.runChildMode benchStr.toName param targetNanos cacheMode env?

/-! ## Cmd tree definitions -/

def listSub : Cmd := `[Cli|
  list VIA runListCmd; ["0.1.0"]
  "List registered benchmarks. Use --tag/--filter to narrow the output."

  FLAGS:
    tag : String;      "Filter by tag (comma-separated for multiple; OR logic). Example: --tag sort,fast"
    filter : String;   "Filter by name substring. Example: --filter Sort"
]

def runSub : Cmd := `[Cli|
  run VIA runRunCmd; ["0.1.0"]
  "Run registered benchmarks; print the result table.

Accepts explicit benchmark names as positional arguments, or use
--tag/--filter to select benchmarks by metadata.

Dispatches by registration kind (parametric vs fixed). Override flags
that don't apply to the dispatched kind are silently ignored, so
`--repeats` on a parametric benchmark and `--slope-tolerance` on a
fixed benchmark are both no-ops.

Override the benchmark's declared config per-run with the flags below.
Each missing flag leaves the declared value untouched."

  FLAGS:
    tag : String;      "Filter by tag (comma-separated for multiple; OR logic). Example: --tag sort,fast"
    filter : String;   "Filter by name substring. Example: --filter Sort"
    "max-seconds-per-call" : Float;  "Hard wallclock cap per batch / per call (seconds; e.g. 0.5; JSON-style numbers)."
    "target-inner-nanos" : Nat;      "Parametric only: auto-tune target wall-time per batch (nanoseconds)."
    "param-floor" : Nat;             "Parametric only: lowest param the doubling ladder starts from."
    "param-ceiling" : Nat;           "Parametric only: highest param the doubling ladder reaches before stopping."
    "warmup-fraction" : Float;       "Parametric only: fraction of leading ratios to drop before computing the verdict (e.g. 0.2; JSON-style numbers)."
    "slope-tolerance" : Float;       "Parametric only: verdict is `consistent` iff |β| ≤ this, where β is the log-log slope of C vs param (JSON-style numbers)."
    "param-schedule" : LeanBench.ParamSchedule;  "Parametric only: ladder shape (auto, doubling, or linear). Default auto picks doubling for polynomial growth, linear for exponential."
    "cache-mode" : LeanBench.CacheMode;           "Parametric only: warm (default) auto-tunes inner repeats inside one child; cold respawns per measurement so cache state is not preserved across rungs. See doc/advanced.md#cache-modes."
    "outer-trials" : Nat;            "Parametric only: number of independent outer trials per ladder rung (default 1). Bumping this above 1 runs N child spawns per param and reports per-param median / min / max / spread; trades runtime for stability. See doc/advanced.md#outer-trials."
    "auto-fit";                      "Parametric only: after the verdict, fit a fixed catalog of complexity models (1, n, n*log n, n^2, n^3, 2^n) to the observed per-call timings and print a ranked suggestion. Heuristic, not a proof. See doc/quickstart.md#auto-fit."
    "repeats" : Nat;                 "Fixed only: number of measured invocations after the warmup call (default 5)."
    "export-file" : String;           "Write results to FILE in machine-readable JSON format (issue #3)."
    baseline : String;               "Compare against a previous export FILE; report regressions and improvements. Exit code is non-zero when any regression exceeds the threshold."
    "regression-threshold" : Float;  "Percentage threshold for flagging regressions (default 10.0; e.g. 10 means >10% slower is a regression)."
    "total-seconds" : Float;         "CI-budget mode (issue #9): bound the whole suite to this many wallclock seconds. Benchmarks that would start past the deadline are recorded as 'budget skip' rows; a benchmark whose ladder runs into the deadline is truncated between rungs. Only meaningful with --tag/--filter (suite mode)."

  ARGS:
    ...names : String;     "Benchmark name(s). If omitted, use --tag/--filter to select."
]

def childSub : Cmd := `[Cli|
  _child VIA runChildCmd; ["0.1.0"]
  "Internal: child-mode single-batch runner. Not intended for direct use."

  FLAGS:
    bench : String;        "Benchmark name to dispatch."
    param : Nat;           "Parametric: parameter value to invoke the function with."
    "target-nanos" : Nat;  "Parametric: inner-tuning target wall-time (ns)."
    "cache-mode" : LeanBench.CacheMode;
                           "Parametric: warm (default — auto-tune inside this child) or cold (single untuned invocation; parent respawns per rung)."
    fixed;                 "Fixed: dispatch the fixed-benchmark single-invocation runner instead of the parametric autotuner."
    "repeat-index" : Nat;  "Fixed: 0-based repeat index to record on the emitted JSONL row."
    "env-json" : String;   "Issue #11: parent's pre-captured env JSON, propagated so all children stamp identical env on their rows. Falls back to fresh capture when absent or malformed."
]

def compareSub : Cmd := `[Cli|
  compare VIA runCompareCmd; ["0.1.0"]
  "Compare multiple registered benchmarks side-by-side.

All listed benchmarks must be the same kind (parametric or fixed);
mixing is rejected. Same per-run override flags as `run`; they apply
to every benchmark in the comparison.

Use --tag/--filter instead of explicit names to select benchmarks by
metadata. When both explicit names and filters are given, only the
explicit names are used."

  FLAGS:
    tag : String;      "Filter by tag (comma-separated for multiple; OR logic). Example: --tag sort,fast"
    filter : String;   "Filter by name substring. Example: --filter Sort"
    "max-seconds-per-call" : Float;  "Hard wallclock cap per batch / per call (seconds; e.g. 0.5; JSON-style numbers)."
    "target-inner-nanos" : Nat;      "Parametric only: auto-tune target wall-time per batch (nanoseconds)."
    "param-floor" : Nat;             "Parametric only: lowest param the doubling ladder starts from."
    "param-ceiling" : Nat;           "Parametric only: highest param the doubling ladder reaches before stopping."
    "warmup-fraction" : Float;       "Parametric only: fraction of leading ratios to drop before computing the verdict (e.g. 0.2; JSON-style numbers)."
    "slope-tolerance" : Float;       "Parametric only: verdict is `consistent` iff |β| ≤ this, where β is the log-log slope of C vs param (JSON-style numbers)."
    "param-schedule" : LeanBench.ParamSchedule;  "Parametric only: ladder shape (auto, doubling, or linear). Default auto picks doubling for polynomial growth, linear for exponential."
    "cache-mode" : LeanBench.CacheMode;           "Parametric only: warm (default) auto-tunes inner repeats inside one child; cold respawns per measurement so cache state is not preserved across rungs. See doc/advanced.md#cache-modes."
    "outer-trials" : Nat;            "Parametric only: number of independent outer trials per ladder rung (default 1). Bumping this above 1 runs N child spawns per param and reports per-param median / min / max / spread; trades runtime for stability. See doc/advanced.md#outer-trials."
    "auto-fit";                      "Parametric only: after each per-function verdict, fit a fixed catalog of complexity models to the observed timings and print a ranked suggestion. Heuristic, not a proof. See doc/quickstart.md#auto-fit."
    "repeats" : Nat;                 "Fixed only: number of measured invocations after the warmup call (default 5)."
    "export-file" : String;           "Write results to FILE in machine-readable JSON format (issue #3)."

  ARGS:
    ...names : String;     "Two or more benchmark names (variadic). Or use --tag/--filter to select."
]

def profileSub : Cmd := `[Cli|
  profile VIA runProfileCmd; ["0.1.0"]
  "Run a single benchmark invocation under an external profiler.

The harness re-spawns itself in child mode at one param, but with the
user-supplied --profiler command prefixed in front. The profiler's
output (perf record's perf.data, perf stat's summary, samply's
server URL, time -v's resource usage, …) lands directly on the user's
terminal alongside the child's JSONL row.

Single-shot by design: profile one param at a time. No ladder, no
verdict, no kill-on-cap (profilers can be slow to flush their own
output). The benchmark's declared cacheMode is honoured — pin
`--cache-mode cold` for one-shot first-touch profiling, leave it at
`warm` (default) so the autotuner exercises the function many times
inside one profiler invocation. See doc/profiling.md for end-to-end
workflows with perf, samply, heaptrack, and time -v.

Linux-first: --profiler is opaque, so any tool the user has on PATH
works, but the canned workflows in doc/profiling.md focus on perf
and samply on Linux. macOS users have Instruments and dtrace; Windows
users have ETW. The harness side is platform-neutral."

  FLAGS:
    profiler : String;               "Required. Profiler command prefix to wrap the child invocation (e.g. \"perf stat --\", \"perf record -F 99 -g --\", \"samply record --\", \"/usr/bin/time -v\"). Whitespace-split into argv tokens; no shell quoting."
    param : Nat;                     "Param value to invoke the function with. Default 0."
    "max-seconds-per-call" : Float;  "Override the declared per-batch cap (seconds). Note: there is no kill-on-cap in profile mode; this only flows through to the child's reporting."
    "target-inner-nanos" : Nat;      "Override the auto-tuner target wall-time per batch (ns). In warm mode (default) this controls how many iterations run inside one profiler invocation."
    "cache-mode" : LeanBench.CacheMode;  "warm (default — autotuned, many calls per profiler invocation) or cold (one untuned call per spawn)."

  ARGS:
    name : String;     "Benchmark name (as registered)."
]

def verifySub : Cmd := `[Cli|
  verify VIA runVerifyCmd; ["0.1.0"]
  "Sanity-check registered benchmarks (f 0, f 1 via the child path). Verifies all benchmarks if no names or filters are given. Exit code is non-zero on any failure."

  FLAGS:
    tag : String;      "Filter by tag (comma-separated for multiple; OR logic). Example: --tag sort,fast"
    filter : String;   "Filter by name substring. Example: --filter Sort"

  ARGS:
    ...names : String;     "Optional: benchmark names to verify; verify all if omitted."
]

/-- Top-level dispatcher; the user calls this as their `main`. -/
def topCmd : Cmd := `[Cli|
  bench NOOP; ["0.1.0"]
  "lean-bench: microbenchmarks for Lean 4. v0.1, Linux/macOS only."

  SUBCOMMANDS:
    listSub;
    runSub;
    compareSub;
    verifySub;
    profileSub;
    childSub
]

def dispatch (args : List String) : IO UInt32 :=
  -- Default subcommand when invoked with no args: `list`.
  let argv := if args.isEmpty then ["list"] else args
  topCmd.validate argv

end Cli
end LeanBench
