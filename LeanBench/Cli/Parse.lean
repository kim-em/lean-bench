import Cli
import Lean.Data.Json
import LeanBench.Core
import LeanBench.Env

/-!
# `LeanBench.Cli.Parse` — flag parsers, override builders, filtering

CLI plumbing shared across all subcommand handlers:

- `Cli.ParseableType` instances for `Float`, `ParamSchedule`, `CacheMode`
  (mhuisi/lean4-cli ships only `Nat` / `Int` / `String` / `Bool`).
- `parsedFlag?` / `configOverrideFromParsed` / `fixedConfigOverrideFromParsed`
  to lift CLI flags into `ConfigOverride` / `FixedConfigOverride`.
- Tag- and name-filter machinery (`splitTags`, `matchEntry`, `parseFilters`,
  `filterParametric`, `filterFixed`, `hasFilters`).
- `BudgetSkip` / `BudgetSummary` — CI-budget-mode bookkeeping (issue #9).

Handler implementations live in [`LeanBench.Cli.Handlers`](Handlers.lean);
the top-level `[Cli|...]` Cmd tree and `dispatch` live in
[`LeanBench.Cli`](../Cli.lean).
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
def parsedFlag? (p : Cli.Parsed) (long : String) (τ : Type)
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
    outerTrials?           := parsedFlag? p "outer-trials" Nat
    signalFloorMultiplier? := parsedFlag? p "signal-floor-multiplier" Float }

/-- Build a `FixedConfigOverride` from override flags. Only fields
that share a flag namespace with parametric (`max-seconds-per-call`)
plus the fixed-only `repeats` flag. Other parametric flags
(param-ceiling, slope-tolerance) are ignored; that's deliberate
since they have no meaning for a fixed benchmark. -/
def fixedConfigOverrideFromParsed (p : Cli.Parsed) : FixedConfigOverride :=
  { repeats?            := parsedFlag? p "repeats" Nat
    maxSecondsPerCall?  := parsedFlag? p "max-seconds-per-call" Float
    minTotalSeconds?    := parsedFlag? p "min-total-seconds" Float
    ignoreExpectedHash  := p.hasFlag "ignore-expected-hash" }

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
def parseFilters (p : Cli.Parsed) : Array String × Option String :=
  let tagFilter := parsedFlag? p "tag" String |>.map splitTags |>.getD #[]
  let nameFilter := parsedFlag? p "filter" String
  (tagFilter, nameFilter)

/-- Filter parametric runtime entries by tag/name. -/
def filterParametric (entries : Array RuntimeEntry)
    (tagFilter : Array String) (nameFilter : Option String) :
    Array RuntimeEntry :=
  entries.filter fun e =>
    matchEntry e.spec.name e.spec.config.tags tagFilter nameFilter

/-- Filter fixed runtime entries by tag/name. -/
def filterFixed (entries : Array FixedRuntimeEntry)
    (tagFilter : Array String) (nameFilter : Option String) :
    Array FixedRuntimeEntry :=
  entries.filter fun e =>
    matchEntry e.spec.name e.spec.config.tags tagFilter nameFilter

/-- Are any filter flags set? -/
def hasFilters (tagFilter : Array String) (nameFilter : Option String) :
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
the deadline trips). See `https://kim-em.github.io/lean-bench/Quickstart/`. -/

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

end Cli
end LeanBench
