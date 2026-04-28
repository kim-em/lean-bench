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

/-!
# `LeanBench.Cli` — argv dispatcher

The user's `main` is just `LeanBench.Cli.dispatch`. The same compiled
binary serves as both parent (orchestrator) and child (single-batch
runner). Subcommand layout:

| invocation                                                      | role |
|-----------------------------------------------------------------|------|
| `./bench`                                                       | parent: print all registered benchmark names |
| `./bench list`                                                  | parent: same |
| `./bench run NAME [override flags]`                             | parent: run one benchmark, print report |
| `./bench compare A B C [override flags]`                        | parent: comparison report |
| `./bench verify [NAMES…]`                                        | parent: bounded `f 0` / `f 1` sanity check via children |
| `./bench _child --bench NAME --param N --target-nanos T`        | child: one inner-tuned batch, print one JSONL row, exit |

`run` and `compare` accept run-time `BenchmarkConfig` overrides via
flags (`--max-seconds-per-call`, `--target-inner-nanos`,
`--param-floor`, `--param-ceiling`, `--warmup-fraction`,
`--slope-tolerance`, `--param-schedule`). Each flag corresponds 1:1
with a `ConfigOverride` field; missing flags leave the declared
config untouched.

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

/-! ## Subcommand handlers (parent side) -/

def runListCmd (_ : Cli.Parsed) : IO UInt32 := do
  let pEntries ← allRuntimeEntries
  let fEntries ← allFixedRuntimeEntries
  if pEntries.isEmpty && fEntries.isEmpty then
    IO.println "(no benchmarks registered)"
    return 0
  IO.println "registered benchmarks:"
  for e in pEntries do
    IO.println (Format.fmtSpec e.spec)
  for e in fEntries do
    IO.println (Format.fmtFixedSpec e.spec)
  return 0

/-- Shared post-run logic: baseline comparison + export. Returns
the exit code (non-zero on regressions). Issue #3. -/
private def handleBaselineAndExport
    (parametric : Array BenchmarkResult)
    (fixed : Array FixedResult)
    (env? : Option Env)
    (baselinePath? exportPath? : Option String)
    (threshold : Float) : IO UInt32 := do
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
    let json := Export.toJsonWithBaseline parametric fixed env? baselineReports?
    IO.FS.writeFile ep (json.pretty ++ "\n")
    IO.println s!"exported to {ep}"
  | none => pure ()
  return exitCode

def runRunCmd (p : Cli.Parsed) : IO UInt32 := do
  let nameStr := (p.positionalArg! "name").as! String
  let name := nameStr.toName
  let exportPath? := parsedFlag? p "export-file" String
  let baselinePath? := parsedFlag? p "baseline" String
  let threshold : Float :=
    (parsedFlag? p "regression-threshold" Float).getD 10.0
  match ← findRuntimeEntry name with
  | some _ =>
    let result ← LeanBench.runBenchmark name (configOverrideFromParsed p)
    IO.println (Format.fmtResult result)
    handleBaselineAndExport #[result] #[] result.env?
      baselinePath? exportPath? threshold
  | none =>
    match ← findFixedRuntimeEntry name with
    | some _ =>
      let result ← LeanBench.runFixedBenchmark name
                      (fixedConfigOverrideFromParsed p)
      IO.println (Format.fmtFixedResult result)
      handleBaselineAndExport #[] #[result] result.env?
        baselinePath? exportPath? threshold
    | none =>
      IO.eprintln s!"run: unregistered benchmark: {name}"
      return 1

/-- Dispatch `compare A B …` by registration kind. All listed names
    must be the same kind (all parametric or all fixed); mixing is
    rejected with a user-facing error. -/
def runCompareCmd (p : Cli.Parsed) : IO UInt32 := do
  let names := p.variableArgsAs! String |>.toList
  if names.isEmpty then
    IO.eprintln "compare: need at least two benchmark names"
    return 1
  let resolved := names.map String.toName
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

def runVerifyCmd (p : Cli.Parsed) : IO UInt32 := do
  -- Use `String.toName` so qualified names like `My.Bench.foo` resolve.
  -- `Lean.Name.mkSimple` would package the whole dotted string into one
  -- atomic name, which never matches anything in the registry.
  let names := p.variableArgsAs! String |>.toList |>.map String.toName
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
  -- `--env-json` is the issue #11 plumbing: when the parent has
  -- already captured env, it serializes via this flag so the child
  -- doesn't redo the work. Parse failures fall back to "let the
  -- child capture fresh" rather than aborting — a malformed env
  -- payload is a parent bug, but the run itself can still proceed.
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
  -- The `--fixed` flag is a discriminator: when present the child
  -- runs a fixed-benchmark single invocation and reads
  -- `--repeat-index`; otherwise it's the existing parametric path
  -- and reads `--param` / `--target-nanos`.
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
  "List registered benchmarks."
]

def runSub : Cmd := `[Cli|
  run VIA runRunCmd; ["0.1.0"]
  "Run a single registered benchmark; print the result table.

Dispatches by registration kind (parametric vs fixed). Override flags
that don't apply to the dispatched kind are silently ignored, so
`--repeats` on a parametric benchmark and `--slope-tolerance` on a
fixed benchmark are both no-ops.

Override the benchmark's declared config per-run with the flags below.
Each missing flag leaves the declared value untouched."

  FLAGS:
    "max-seconds-per-call" : Float;  "Hard wallclock cap per batch / per call (seconds; e.g. 0.5; JSON-style numbers)."
    "target-inner-nanos" : Nat;      "Parametric only: auto-tune target wall-time per batch (nanoseconds)."
    "param-floor" : Nat;             "Parametric only: lowest param the doubling ladder starts from."
    "param-ceiling" : Nat;           "Parametric only: highest param the doubling ladder reaches before stopping."
    "warmup-fraction" : Float;       "Parametric only: fraction of leading ratios to drop before computing the verdict (e.g. 0.2; JSON-style numbers)."
    "slope-tolerance" : Float;       "Parametric only: verdict is `consistent` iff |β| ≤ this, where β is the log-log slope of C vs param (JSON-style numbers)."
    "param-schedule" : LeanBench.ParamSchedule;  "Parametric only: ladder shape (auto, doubling, or linear). Default auto picks doubling for polynomial growth, linear for exponential."
    "cache-mode" : LeanBench.CacheMode;           "Parametric only: warm (default) auto-tunes inner repeats inside one child; cold respawns per measurement so cache state is not preserved across rungs. See doc/advanced.md#cache-modes."
    "outer-trials" : Nat;            "Parametric only: number of independent outer trials per ladder rung (default 1). Bumping this above 1 runs N child spawns per param and reports per-param median / min / max / spread; trades runtime for stability. See doc/advanced.md#outer-trials."
    "repeats" : Nat;                 "Fixed only: number of measured invocations after the warmup call (default 5)."
    "export-file" : String;           "Write results to FILE in machine-readable JSON format (issue #3)."
    baseline : String;               "Compare against a previous export FILE; report regressions and improvements. Exit code is non-zero when any regression exceeds the threshold."
    "regression-threshold" : Float;  "Percentage threshold for flagging regressions (default 10.0; e.g. 10 means >10% slower is a regression)."

  ARGS:
    name : String;     "Benchmark name (lowercase, as registered)."
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
to every benchmark in the comparison."

  FLAGS:
    "max-seconds-per-call" : Float;  "Hard wallclock cap per batch / per call (seconds; e.g. 0.5; JSON-style numbers)."
    "target-inner-nanos" : Nat;      "Parametric only: auto-tune target wall-time per batch (nanoseconds)."
    "param-floor" : Nat;             "Parametric only: lowest param the doubling ladder starts from."
    "param-ceiling" : Nat;           "Parametric only: highest param the doubling ladder reaches before stopping."
    "warmup-fraction" : Float;       "Parametric only: fraction of leading ratios to drop before computing the verdict (e.g. 0.2; JSON-style numbers)."
    "slope-tolerance" : Float;       "Parametric only: verdict is `consistent` iff |β| ≤ this, where β is the log-log slope of C vs param (JSON-style numbers)."
    "param-schedule" : LeanBench.ParamSchedule;  "Parametric only: ladder shape (auto, doubling, or linear). Default auto picks doubling for polynomial growth, linear for exponential."
    "cache-mode" : LeanBench.CacheMode;           "Parametric only: warm (default) auto-tunes inner repeats inside one child; cold respawns per measurement so cache state is not preserved across rungs. See doc/advanced.md#cache-modes."
    "outer-trials" : Nat;            "Parametric only: number of independent outer trials per ladder rung (default 1). Bumping this above 1 runs N child spawns per param and reports per-param median / min / max / spread; trades runtime for stability. See doc/advanced.md#outer-trials."
    "repeats" : Nat;                 "Fixed only: number of measured invocations after the warmup call (default 5)."
    "export-file" : String;           "Write results to FILE in machine-readable JSON format (issue #3)."

  ARGS:
    ...names : String;     "Two or more benchmark names (variadic)."
]

def verifySub : Cmd := `[Cli|
  verify VIA runVerifyCmd; ["0.1.0"]
  "Sanity-check registered benchmarks (f 0, f 1 via the child path). Verifies all benchmarks if no names are given. Exit code is non-zero on any failure."

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
    childSub
]

def dispatch (args : List String) : IO UInt32 :=
  -- Default subcommand when invoked with no args: `list`.
  let argv := if args.isEmpty then ["list"] else args
  topCmd.validate argv

end Cli
end LeanBench
