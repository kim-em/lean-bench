import Lean.Data.Name
import Std.Data.HashSet

/-!
# `LeanBench.Core` — shared types

Defined here so `Setup`, `Run`, `Child`, `Stats`, `Compare`, and
`Format` can all import a single ground-truth source for the data
shapes. No runtime behaviour in this file.
-/

open Lean (Name)

namespace LeanBench

/-! ## Reproducibility metadata

Captured once per process (by the child for emission on each JSONL
row, by the parent for the in-memory `BenchmarkResult`). The data
shape is `Inhabited` so callers that don't capture env (tests,
synthesized error rows) get a structural default rather than
threading an `Option` through every code path; the field on the
in-memory result *is* an `Option` so "we never captured" stays
distinguishable from "we captured but every field was unknown."

Field semantics live in `doc/schema.md#environment-metadata`. Fields
typed `Option α` are best-effort: we capture them when we can and
emit an explicit `null` rather than silently omitting (per the
issue #11 acceptance criterion). Always-present fields are
derivable from Lean compile-time constants (`Lean.versionString`,
`System.Platform.target`) so no probing is needed for them. -/
structure Env where
  /-- `Lean.versionString` — e.g. `"4.30.0-rc2"` or
      `"4.31.0, commit abc123"` for development builds. -/
  leanVersion       : String
  /-- `Lean.toolchain` — e.g. `"leanprover/lean4:v4.30.0-rc2"`.
      Matches the `lean-toolchain` file format. -/
  leanToolchain     : String
  /-- LLVM target triple (`System.Platform.target`), e.g.
      `"x86_64-unknown-linux-gnu"`. Empty string when missing —
      Lean records it as `""` in that case, and we propagate the
      empty string verbatim rather than fabricating a `null`. -/
  platformTarget    : String
  /-- Coarse OS family: `"linux"` / `"macos"` / `"windows"` /
      `"emscripten"`. The detector defaults to `"linux"` for any
      non-{Windows, macOS, Emscripten} platform; readers should
      treat this as informational, not authoritative. -/
  os                : String
  /-- Architecture parsed from the target triple's leading segment
      (`"x86_64"`, `"aarch64"`, …). `none` when the triple is empty
      or unparseable. -/
  arch              : Option String
  /-- Human-readable CPU model string (`"Intel(R) Xeon(R) ..."`).
      `none` when we can't read it (non-Linux without subprocess
      probing, restricted permissions, etc.). -/
  cpuModel          : Option String
  /-- Logical core count as the OS sees it. `none` when unavailable. -/
  cpuCores          : Option Nat
  /-- Hostname as reported by `gethostname` (via the `HOSTNAME` /
      `COMPUTERNAME` env var fallback when the syscall is
      unavailable). `none` when neither is readable. -/
  hostname          : Option String
  /-- Basename of `IO.appPath` — the running benchmark executable. -/
  exeName           : String
  /-- The lean-bench library version, pinned by
      `LeanBench.libraryVersion`. -/
  leanBenchVersion  : String
  /-- Resolved `git rev-parse HEAD` from the current working
      directory (full 40-char SHA). `none` when `git` is unavailable
      or cwd isn't a git repo. -/
  gitCommit         : Option String
  /-- True iff the working tree had uncommitted changes at capture
      time (`git status --porcelain` non-empty). `none` when git
      isn't available — distinct from `some false`, which means we
      checked and the tree was clean. -/
  gitDirty          : Option Bool
  /-- Wallclock at capture time, milliseconds since the Unix epoch.
      `Int` because `Std.Time.Timestamp.now` returns a signed offset
      and we're not in the business of asserting a non-negative
      epoch. -/
  timestampUnixMs   : Int
  /-- Capture-time wallclock as an ISO 8601 UTC string
      (`"YYYY-MM-DDTHH:MM:SSZ"`). Derived from `timestampUnixMs` so
      readers can choose either; producers MUST keep the two
      consistent. -/
  timestampIso      : String
  deriving Repr, Inhabited

/-- Library version string. Kept as a single source of truth so
docs, JSONL rows, and the user-facing report all agree. Bumped at
release time alongside `lakefile.toml`. -/
def libraryVersion : String := "0.1.0"

/-- Status of a single measurement. Mirrors the `status` JSONL field. -/
inductive Status
  | ok
  | timedOut
  | killedAtCap
  | error (msg : String)
  deriving Repr, Inhabited, BEq

def Status.toJsonString : Status → String
  | .ok => "ok"
  | .timedOut => "timed_out"
  | .killedAtCap => "killed_at_cap"
  | .error _ => "error"

/--
One observation: a single subprocess invocation that ran the function
under test `innerRepeats` times in a row and measured wall time for
the whole batch.
-/
structure DataPoint where
  param        : Nat
  innerRepeats : Nat
  totalNanos   : Nat
  /-- `totalNanos / innerRepeats` precomputed for convenience. -/
  perCallNanos : Float
  /-- Present iff the function's return type has a `Hashable` instance. -/
  resultHash   : Option UInt64
  status       : Status
  /-- False for doubling-probe rows in `.linear` mode: they exist to
      bracket the productive band but must not enter the verdict (else
      cold-regime rows corrupt `cMin/cMax`/slope). True for everything
      else, including all rows in `.doubling` mode. Parent-side only —
      not serialized in the child JSONL. -/
  partOfVerdict : Bool := true
  /-- True when this row's `totalNanos` is below
      `signalFloorMultiplier × spawnFloorNanos`, i.e. the entire
      measurement is small enough that subprocess-spawn cost dominates
      the inner-loop work. Such rows are unreliable as algorithm
      timing data — they are excluded from the verdict reduction
      (see `Stats.ratiosFromPoints`) and rendered with a `[<floor]`
      annotation in the report. Parent-side only; computed after the
      spawn-floor self-measurement so the child JSONL can stay
      schema-stable. -/
  belowSignalFloor : Bool := false
  /-- 0-based index across the `outerTrials` independent measurements
      taken at this `param`. `0` for benchmarks declared with the v0.1
      default `outerTrials := 1` (so all existing fixtures parse
      unchanged). Parent-side only — not serialized in the child JSONL;
      the parent assigns it after each spawn returns. Issue #4. -/
  trialIndex   : Nat := 0
  /-- Total bytes allocated by the Lean runtime over the child
      process's lifetime, captured at the end of the measurement.
      `none` on every platform today — Lean 4 has no portable
      in-process API to read this. The field exists so the wire
      format, parsers, and report renderers all have a place for the
      value to land once a future Lean release exposes allocation
      counters. Issue #6. -/
  allocBytes : Option Nat := none
  /-- Peak resident-set size in kibibytes for the child process at
      the end of the measurement. Captured on Linux from
      `/proc/self/status`'s `VmHWM:` line; `none` on macOS, Windows,
      and any other platform without `/proc`. Best-effort — every
      capture failure (missing file, permission denied) collapses to
      `none` rather than aborting the measurement. Issue #6. -/
  peakRssKb  : Option Nat := none
  deriving Repr, Inhabited

/-- Heuristic verdict — see SPEC v0.1: weak labels by design. Decided
by fitting the log-log slope β of `C = perCallNanos / complexity(n)` vs
`param` on the trimmed tail: `|β| ≤ slopeTolerance` → consistent, else
inconclusive. The slope itself (carrying the direction) lives on
`BenchmarkResult.slope?`; the verdict is intentionally two-valued. -/
inductive Verdict
  | consistentWithDeclaredComplexity
  | inconclusive
  deriving Repr, Inhabited, BEq

def Verdict.describe : Verdict → String
  | .consistentWithDeclaredComplexity => "consistent with declared complexity"
  | .inconclusive => "inconclusive"

/-- Shape of the parameter ladder.

    `.doubling` is the right schedule for polynomial complexity: it
    spreads samples evenly in log-space so the verdict's log-log slope
    fit is well-conditioned.

    `.linear` is the right schedule for exponential complexity: a
    doubling probe brackets the productive band (between the last
    `ok` rung and the first capped rung), then `samples` rungs are
    run inside that bracket so the high-`n` regime where the model
    dominates overhead is well-sampled.

    `.custom` runs an explicit user-specified list of params, in
    order, with no doubling probe and no auto-bracketing. Right for
    workloads where the natural inputs are non-uniform (e.g. file
    sizes from a corpus) or where the user already knows which
    `n` matter — see issue #15. The harness still applies the
    wallclock cap and stops as soon as a row hits it.

    `.auto` is the default — at runtime it inspects the declared
    complexity and resolves to `.doubling` or `.linear`. The
    heuristic compares `complexity(2n)/complexity(n)` at two scales:
    for any `n^k` it is constant (→ doubling); for any `b^n` it
    grows geometrically (→ linear). `.custom` is never auto-picked;
    it is opt-in only via declaration-time
    `where { paramSchedule := .custom #[...] }`. -/
inductive ParamSchedule
  | doubling
  | linear (samples : Nat := 16)
  | custom (params : Array Nat)
  | auto
  deriving Inhabited, Repr, BEq

/-- Cache-state policy for a parametric benchmark.

`.warm` (the default) is the current behaviour: the child auto-tunes
an inner-repeat count, runs the function many times in a single
spawn, and reports per-call time over a steady-state batch. CPU
caches, branch predictors, JIT-style code paths in the runtime, and
the GC's live set are all primed across the repeats — what's
measured is the asymptotic cost of the algorithm under hot
microarchitectural state.

`.cold` respawns the child for every ladder rung and runs the
function exactly once per spawn, so the harness no longer
intentionally preserves intra-child state across measurements.
What's measured includes cache refill, branch-predictor warmup,
allocator first-touch, and any per-call overhead the warm path
amortises away. Note that "cold" here means "no harness-side
warmup," not "guaranteed cold hardware" — the OS / CPU may still
carry parts of the working set across spawns; see
`doc/advanced.md#cache-modes`.

Neither mode is "more correct" — they measure different things. Read
the docs in `doc/advanced.md#cache-modes` for when each is
appropriate. The wire format records the mode used on every
parametric JSONL row so post-hoc analysis can keep them apart. -/
inductive CacheMode
  | warm
  | cold
  deriving Inhabited, Repr, BEq

def CacheMode.toJsonString : CacheMode → String
  | .warm => "warm"
  | .cold => "cold"

/-- Per-benchmark configuration. Defaults are the v0.1 contract.

Override defaults in two ways:

- At declaration time via `setup_benchmark name n => model where { … }`
  — see `LeanBench.Setup`.
- At run time via CLI flags on `run` / `compare` (e.g.
  `--max-seconds-per-call 0.25 --param-ceiling 1024`) — see
  `LeanBench.Cli`.

The structure deliberately keeps every field optional (default-valued) so
both paths use the same builder syntax and new fields can be added
without breaking existing benchmarks. Future measurement-mode flags
(see #12) plug in as additional fields here. -/
structure BenchmarkConfig where
  /-- Target wall time per inner-tuned batch (ns). Default 500ms. -/
  targetInnerNanos  : Nat   := 500_000_000
  /-- Hard wallclock cap for any single batch (s). Parent kills child
      past this. `Float` so users can pass fractional values such as
      `0.25` from CLI overrides. -/
  maxSecondsPerCall : Float := 1.0
  /-- Max param the doubling ladder will reach before stopping. -/
  paramCeiling      : Nat   := 1_073_741_824
  /-- Min param the doubling ladder starts from. -/
  paramFloor        : Nat   := 0
  /-- Grace ms between SIGTERM and SIGKILL on the child. -/
  killGraceMs       : Nat   := 100
  /-- Fraction of leading ratios to drop before computing cMin/cMax for
      the verdict. `0.0` uses every ratio; `0.2` drops the first 20% of
      ratio samples (the cold regime, where per-call overhead dominates
      the declared complexity). The raw `ratios` array in the result is
      not trimmed — only the verdict reduction sees the trimmed view.
      Clamped so that at least 3 samples remain. -/
  verdictWarmupFraction : Float := 0.2
  /-- Tolerance on the log-log slope β of `C` vs `param` over the
      trimmed tail. The verdict is "consistent with declared
      complexity" iff `|β| ≤ slopeTolerance`, otherwise "inconclusive".
      Default 0.15 tolerates realistic per-point noise (~5% across ~10
      tail rungs) while still flagging a spurious polynomial factor
      like `n log n` vs declared `n`. Widen on chronically noisy
      hardware; tighten only if you trust the measurements. -/
  slopeTolerance : Float := 0.15
  /-- Noise tolerance on the multiplicative `cMax / cMin` range used
      as a fallback verdict on narrow ladders (where the slope fit is
      ill-conditioned). The actual bound combines this floor with the
      scale-adjusted slope tolerance:
      `cMax / cMin ≤ max(narrowRangeNoiseFloor, exp(slopeTolerance · xRange))`.
      Default 1.50 admits the 15–25% spread observed at near-cap
      `×2^0` rungs of an exponential-complexity linear ladder (where
      single-shot timing variance is the dominant noise source) with
      enough headroom for noisy hardware. Tighten if you want to
      discriminate finer model differences and accept a higher
      false-negative rate; widen further on chronically noisy hosts. -/
  narrowRangeNoiseFloor : Float := 1.50
  /-- Ladder shape — see `ParamSchedule`. `.auto` (default) inspects
      the declared complexity at runtime and picks `.doubling` for
      polynomial growth, `.linear` for exponential. -/
  paramSchedule : ParamSchedule := .auto
  /-- Cache-state policy — see `CacheMode`. `.warm` (default) is the
      current inner-repeat-in-child design; `.cold` respawns for every
      rung and runs the function exactly once per spawn so cache state
      is not preserved across measurements. The two modes measure
      different things; see `doc/advanced.md#cache-modes`. -/
  cacheMode : CacheMode := .warm
  /-- Multiplier on the per-spawn floor below which a row's
      `totalNanos` is treated as dominated by subprocess overhead
      rather than algorithm work. Rows below this threshold are
      flagged `belowSignalFloor` (rendered with a `[<floor]`
      annotation) and excluded from the verdict reduction. Default
      `10.0` matches the rule of thumb in `doc/quickstart.md` ("any
      data point with total_nanos smaller than ~10× the spawn floor
      is noise"). Set to `1.0` to disable the check (every row is
      retained); values ≤ 0 are rejected by `validate`. See issue
      #15 for the design discussion. -/
  signalFloorMultiplier : Float := 10.0
  /-- Number of independent outer trials per measured rung. `1` (the
      default) is the v0.1 behaviour: one batch per param. Bumping
      this above 1 runs `outerTrials` separate child spawns per rung
      and aggregates the per-call timings into a per-param median /
      min / max / spread on `BenchmarkResult.trialSummaries`. The
      raw per-trial points are preserved on `BenchmarkResult.points`
      so downstream tooling and the divergence reporter stay
      apples-to-apples. The verdict ratios are computed from the
      median per param, so a single noisy trial no longer shifts the
      slope fit on its own. Trade-off: total wall time scales linearly
      with `outerTrials`. CI tunes this via `--outer-trials` to trade
      runtime for stability. Issue #4. -/
  outerTrials : Nat := 1
  /-- User-defined tags for organizing benchmarks into groups. Tags
      are arbitrary strings attached at declaration time via
      `where { tags := #["sort", "fast"] }`. The CLI can filter by tag
      (`--tag sort`) or by name substring (`--filter Sort`) on `list`,
      `run`, `compare`, and `verify`. Tags are metadata — they don't
      affect measurement, the verdict, or the wire format. Issue #10. -/
  tags : Array String := #[]
  deriving Inhabited, Repr, BEq

/-- Optional run-time overrides applied on top of a benchmark's
declared `BenchmarkConfig`. Each `none` field leaves the declared
value untouched; each `some` replaces it.

Built by the CLI from flags such as `--max-seconds-per-call 0.5
--param-ceiling 1024 --warmup-fraction 0.3`. Kept as a separate type
so the macro-time defaults stay independent of CLI plumbing and so
adding a new override knob is a one-field change. The set of fields
here is intentionally narrower than `BenchmarkConfig` — the
noise-floor knob stays declaration-time-only via `where { ... }`
because its sensible value depends on hardware specifics rather than
per-run intent. -/
structure ConfigOverride where
  targetInnerNanos?      : Option Nat   := none
  maxSecondsPerCall?     : Option Float := none
  paramCeiling?          : Option Nat   := none
  paramFloor?            : Option Nat   := none
  verdictWarmupFraction? : Option Float := none
  slopeTolerance?        : Option Float := none
  killGraceMs?           : Option Nat   := none
  /-- Override the ladder shape. CLI exposes this as
      `--param-schedule auto|doubling|linear`. The `.linear` form
      has a `samples` count that the CLI cannot express, so
      `ConfigOverride.apply` carries the declared sample count
      forward when both the declared and CLI shapes are `.linear`
      — pin a non-default count via
      `where { paramSchedule := .linear 32 }` and the CLI flag
      becomes a no-op (the shape is already linear). When the CLI
      switches the shape (e.g. declared `.doubling`, CLI `.linear`),
      the macro-default sample count is used. -/
  paramSchedule?         : Option ParamSchedule := none
  /-- Override the cache-state policy. CLI exposes this as
      `--cache-mode warm|cold`. -/
  cacheMode?             : Option CacheMode := none
  /-- Override the number of outer trials per measured rung. CLI
      exposes this as `--outer-trials N`. Issue #4. -/
  outerTrials?           : Option Nat := none
  /-- Override the per-spawn signal-floor multiplier. CLI exposes
      this as `--signal-floor-multiplier`. `1.0` disables the filter
      entirely (no rows are flagged `[<floor]`); higher values are
      more aggressive about excluding cold rungs. Useful in CI smoke
      tests on slow runners where the per-spawn floor would otherwise
      swallow every rung at the chosen `--param-ceiling`. Issue #47. -/
  signalFloorMultiplier? : Option Float := none
  deriving Inhabited, Repr

/-- Merge a CLI `paramSchedule` override on top of a declared
schedule. `--param-schedule linear` only carries the schedule kind
(no CLI surface for the `samples` count); when the declared config
is already `.linear n`, preserve `n` rather than clobbering it with
the parser's macro-default 16. Switching shape (e.g. declared
`.doubling`, CLI `.linear`) lands on the macro default since the
declared config has no `samples` to carry. -/
private def mergeParamSchedule
    (cli : Option ParamSchedule) (declared : ParamSchedule) :
    ParamSchedule :=
  match cli, declared with
  | none, _ => declared
  | some (.linear _), .linear n => .linear n
  | some s, _ => s

/-- Apply overrides on top of a config. `none` keeps the config value. -/
def ConfigOverride.apply (o : ConfigOverride) (c : BenchmarkConfig) :
    BenchmarkConfig :=
  -- `{ c with ... }` so fields not present in `ConfigOverride`
  -- (`narrowRangeNoiseFloor`) are preserved from the declared
  -- config rather than reset to structure defaults.
  { c with
    targetInnerNanos      := o.targetInnerNanos?.getD      c.targetInnerNanos
    maxSecondsPerCall     := o.maxSecondsPerCall?.getD     c.maxSecondsPerCall
    paramCeiling          := o.paramCeiling?.getD          c.paramCeiling
    paramFloor            := o.paramFloor?.getD            c.paramFloor
    verdictWarmupFraction := o.verdictWarmupFraction?.getD c.verdictWarmupFraction
    slopeTolerance        := o.slopeTolerance?.getD        c.slopeTolerance
    killGraceMs           := o.killGraceMs?.getD           c.killGraceMs
    paramSchedule         := mergeParamSchedule o.paramSchedule? c.paramSchedule
    cacheMode             := o.cacheMode?.getD             c.cacheMode
    outerTrials           := o.outerTrials?.getD           c.outerTrials
    signalFloorMultiplier := o.signalFloorMultiplier?.getD c.signalFloorMultiplier }

/-- Validate a `BenchmarkConfig`. The macro accepts arbitrary user
expressions and the CLI accepts arbitrary JSON-style numbers, so
nothing rules out e.g. `maxSecondsPerCall := -1.0` or
`paramFloor > paramCeiling` at construction time. We catch the most
confusing combinations here and report them as a single user-facing
error rather than letting them produce empty ladders, hung children,
or vacuous verdicts further downstream. Returns `Except String Unit`
so the caller decides whether to throw an `IO.Error`, attach a
location, etc. -/
def BenchmarkConfig.validate (c : BenchmarkConfig) : Except String Unit := do
  unless c.maxSecondsPerCall > 0.0 do
    .error s!"BenchmarkConfig.maxSecondsPerCall must be > 0; got {c.maxSecondsPerCall}"
  unless c.targetInnerNanos > 0 do
    .error s!"BenchmarkConfig.targetInnerNanos must be > 0; got {c.targetInnerNanos}"
  unless c.slopeTolerance > 0.0 do
    .error s!"BenchmarkConfig.slopeTolerance must be > 0; got {c.slopeTolerance}"
  unless 0.0 ≤ c.verdictWarmupFraction ∧ c.verdictWarmupFraction < 1.0 do
    .error s!"BenchmarkConfig.verdictWarmupFraction must be in [0, 1); got {c.verdictWarmupFraction}"
  unless c.paramFloor ≤ c.paramCeiling do
    .error s!"BenchmarkConfig.paramFloor must be ≤ paramCeiling; got {c.paramFloor} > {c.paramCeiling}"
  unless c.signalFloorMultiplier ≥ 1.0 do
    .error s!"BenchmarkConfig.signalFloorMultiplier must be ≥ 1.0; got {c.signalFloorMultiplier}"
  unless c.outerTrials ≥ 1 do
    .error s!"BenchmarkConfig.outerTrials must be ≥ 1; got {c.outerTrials}"
  -- A `.custom` ladder with an empty params list is almost certainly
  -- a typo (and would produce an empty result); reject up front.
  -- Duplicate params are also rejected: the formatter keys the `C`
  -- column and the warmup-trim dagger by `param`, so two rows with
  -- the same param would render with the same `C` (and either both
  -- or neither carry the dagger). That makes the report misleading
  -- even when the underlying measurements are fine. Codex flagged
  -- this in the issue #15 review.
  match c.paramSchedule with
  | .custom ps =>
    unless ps.size > 0 do
      .error s!"BenchmarkConfig.paramSchedule = .custom #[] is empty; supply at least one param"
    let dedup : Std.HashSet Nat := ps.foldl (·.insert ·) {}
    unless dedup.size == ps.size do
      .error s!"BenchmarkConfig.paramSchedule = .custom contains duplicate params; got {ps}"
  | _ => pure ()
  pure ()

/-- One entry in the benchmark registry. Stored as `Name`s plus a
printable copy of the complexity formula; neither `Syntax` nor `Expr`,
sidestepping serialization issues. -/
structure BenchmarkSpec where
  /-- The function under test. -/
  name             : Lean.Name
  /-- Auto-generated `Nat → Nat` complexity model. -/
  complexityName   : Lean.Name
  /-- Pretty-printed source of the complexity expression (e.g. `"2 ^ n"`),
      captured at `setup_benchmark` time so `list` can show the formula
      the user wrote rather than the internal helper name. -/
  complexityFormula : String
  /-- Auto-generated `Nat → IO (Option UInt64)` runner. -/
  runCheckedName   : Lean.Name
  /-- Auto-generated `BenchmarkConfig` def carrying any
      `where { ... }` overrides applied at declaration time.
      Compile-time tooling that walks the persistent registry can
      resolve this constant against the environment to recover the
      declared config; the runtime registry already has it as a
      value. -/
  configDeclName   : Lean.Name := Lean.Name.anonymous
  /-- True iff the function's return type has a `Hashable` instance;
      hashing is enabled in `runChecked` only when this is set. -/
  hashable         : Bool
  config           : BenchmarkConfig
  deriving Inhabited, Repr

/-- `(param, C = perCallNanos / complexity param)`. Source of truth
  for the verdict. -/
abbrev Ratio := Nat × Float

/-- Aggregate timing summary for one `param` across all `outerTrials`
independent measurements at that param.

The fields capture the raw shape of the trial cluster — median, min,
max, and a relative-spread number — without committing to any
distributional model. With `outerTrials = 1` the harness still emits
one summary per ok param (median = min = max, spread = 0); the same
shape works for both `--outer-trials 1` and `--outer-trials N`, so
downstream tooling doesn't need a special case.

`perCallNanos` is in nanoseconds. `relativeSpread` is `(max - min) /
median` — `0.10` means the worst trial was 10% above the best
relative to the median. It is intentionally NOT a confidence interval
or standard deviation: the v0.2 measurement model does not commit to
a noise distribution; this is a robust shape descriptor that users
can read at a glance.

Issue #4. -/
structure TrialSummary where
  param          : Nat
  /-- Number of verdict-eligible `ok` trials that contributed to the
      summary. Trials that timed out / were killed / errored are not
      counted, and neither are doubling-probe rows in `.linear`
      mode (`partOfVerdict := false`) nor rows that fell below the
      signal floor (`belowSignalFloor := true`); the summary covers
      the same row set as `Stats.ratiosFromPoints`. The summary may
      have `okCount < BenchmarkConfig.outerTrials`; `0` should not
      occur because the harness emits a `TrialSummary` only when at
      least one trial survived all of those filters. -/
  okCount        : Nat
  /-- Median of `perCallNanos` over the `okCount` trials. Upper
      middle on even sample counts (matches `Stats.medianFloat`). -/
  medianPerCallNanos : Float
  minPerCallNanos    : Float
  maxPerCallNanos    : Float
  /-- `(max - min) / median`, clamped to `0.0` when `median = 0.0`. -/
  relativeSpread     : Float
  deriving Repr, Inhabited

/-- Edge-case classifications surfaced on `BenchmarkResult.advisories`.
Pure data — the format layer turns each into an actionable message.

A single result may carry multiple advisories: e.g. a benchmark whose
function is too fast AND whose ladder hit the cap on the largest rung
gets both `belowSignalFloor` and `truncatedAtCap`. The advisories are
not mutually exclusive and the format renders them in declaration
order. See issue #15.

Parent-side only; not serialized in the child JSONL row. -/
inductive Advisory
  /-- Every successful row was below the signal-floor threshold —
      total wall time was dominated by per-spawn overhead, not the
      algorithm under test. The verdict cannot be trusted; the
      benchmark is too fast for child-process measurement at the
      configured params. -/
  | belowSignalFloor
  /-- `n` rows out of total `total` were below the signal-floor
      threshold but at least one row above it survived. The verdict
      is computed on the surviving rows; the cold leading rungs are
      noted for context. -/
  | partiallyBelowSignalFloor (n total : Nat)
  /-- Every measurement hit the wallclock cap (timed-out or
      killed-at-cap) — no `ok` row landed at all. The benchmark is
      too slow at the configured params; bump `--max-seconds-per-call`
      or lower `--param-ceiling`. -/
  | allCapped
  /-- The ladder produced at least one `ok` row but was truncated by
      the wallclock cap before reaching `paramCeiling`. The
      `firstFail` param is the rung that was killed; the trimmed
      tail above it was never measured. -/
  | truncatedAtCap (firstFail : Nat)
  /-- After applying both the warmup trim and the signal-floor
      filter, fewer than 3 rows remain — the verdict reduction has
      no resolution. Either the ladder is too short or every rung
      is in an edge regime. -/
  | tooFewVerdictRows (kept : Nat)
  deriving Repr, Inhabited, BEq

/-- Result of a single benchmark invocation. -/
structure BenchmarkResult where
  function   : Lean.Name
  /-- The expected-complexity formula the user declared, as printable
      source (e.g. `"2 ^ n"`). Echoed back in the result header so
      reports stay readable without needing the env around. -/
  complexityFormula : String
  /-- True iff the function's return type has a `Hashable` instance —
      mirrored from `BenchmarkSpec.hashable` so cross-result tooling
      (`compare`, exporters) can distinguish "this benchmark has no
      Hashable instance" from "this benchmark's runs all failed
      before a hash was emitted". -/
  hashable   : Bool := false
  config     : BenchmarkConfig
  points     : Array DataPoint
  ratios     : Array Ratio
  verdict    : Verdict
  cMin?      : Option Float
  cMax?      : Option Float
  /-- How many leading ratios were dropped by `verdictWarmupFraction`
      before computing cMin/cMax. `0` when no trimming was applied. -/
  verdictDroppedLeading : Nat := 0
  /-- OLS slope β of `log C` vs `log param` over the trimmed tail.
      Expected ≈ 0 when the declared complexity matches. Positive β
      means the function grows faster than declared by roughly a factor
      of `n^β`; negative β means slower. `none` when the trimmed tail
      has fewer than 2 samples or degenerate `x` (shouldn't happen on
      the doubling ladder). -/
  slope? : Option Float := none
  /-- The harness's own per-spawn floor at the time of this run, for
      comparison against the smallest `totalNanos` recorded. -/
  spawnFloorNanos? : Option Nat := none
  /-- Edge-case advisories surfaced for this run — see `Advisory`.
      Empty when the run looks ordinary (most rows above the signal
      floor, none capped, the ladder ran to completion). The format
      layer renders these as a hint section after the verdict line.
      Issue #15. -/
  advisories : Array Advisory := #[]
  /-- One entry per **verdict-eligible** param, in ladder order — the
      same param set `Stats.ratiosFromPoints` returns. Doubling-probe
      rows in `.linear` mode and rows below the signal floor are
      excluded both here and from the verdict reduction, so this
      array reflects exactly the data the verdict was computed from.
      With `outerTrials := 1` (the v0.1 default) each entry trivially
      has `okCount = 1` and `relativeSpread = 0.0`; with
      `outerTrials > 1` the entry summarises the cluster of
      independent measurements at that param (median / min / max /
      spread). The verdict ratios are computed from
      `medianPerCallNanos` so a single noisy trial doesn't shift the
      slope fit. The full per-trial points (including probe and
      below-floor rows) stay on `points` for downstream tooling and
      raw inspection. Issue #4. -/
  trialSummaries : Array TrialSummary := #[]
  /-- Reproducibility metadata captured at parent run start (issue
      #11). `none` when the result was assembled outside the
      `runBenchmark` path (test fixtures, hand-rolled `Inhabited`
      instances). Children also stamp env on every JSONL row so
      downstream tools that only see the wire format can recover it
      independently. -/
  env? : Option Env := none
  /-- True when the ladder was cut short by a CI-budget deadline
      (`--total-seconds`, issue #9). The recorded `points` reflect the
      rungs that did run before the deadline; the rungs that would
      have followed never ran. The verdict is still computed from
      whatever data landed, so a budget-truncated result is meaningful
      but typically less reliable than a full ladder. Default `false`
      for runs without a budget. -/
  budgetTruncated : Bool := false
  deriving Inhabited, Repr

/-- The benchmark produced no verdict-eligible rows: every measurement
    was below the per-spawn signal floor, every measurement hit the
    wallclock cap, every measurement errored, or only param-0/1 rows
    landed. The declared schedule + wallclock cap + inner-tuning budget
    failed to produce any data the harness can use to compute a
    verdict — a hard calibration failure of the registration, not a
    soft "we tried our best." Issue #47.

    `ratios` is built by `Stats.ratiosFromPoints`, which already filters
    out non-`ok`, doubling-probe, below-floor, and `param < 2` rows, so
    "no verdict-eligible rows" is exactly `ratios.isEmpty`.

    Budget-truncated runs (`--total-seconds`, issue #9) are excluded:
    those are user-requested early stops, not calibration bugs. The
    budget summary already surfaces "completed/skipped/truncated"
    counts on stdout and in the JSON export, which is the right
    signal for that case. -/
def BenchmarkResult.noUsableData (r : BenchmarkResult) : Bool :=
  r.ratios.isEmpty && !r.budgetTruncated

/-- Process exit code that signals "the parametric run produced no
    verdict-eligible rows" — see `BenchmarkResult.noUsableData`. Chosen
    distinct from `1` so scripts and CI can distinguish a calibration
    bug (the harness has nothing to compare) from a baseline
    regression (the harness compared and found a mismatch). Issue #47. -/
def exitNoUsableData : UInt32 := 2

/-- One diverging param in a `compare`: the param at which results
disagreed, the full hash table of each compared function at that
param (in CLI argument order, so reports can render an "all
implementations side-by-side" view), and the names of the
implementations whose hash differed from the baseline (the first
entry in `hashes`). -/
structure DivergenceDetail where
  /-- The shared param at which the disagreement was observed. -/
  param      : Nat
  /-- One entry per compared function, in the order the user passed
      them on the command line. The hash is `Option UInt64` so
      functions that produced no hash at this param (e.g. an early
      `killed_at_cap` row) still appear in the table — the formatter
      shows them as `hash=—` to make the absence visible. The first
      entry is the comparison baseline. -/
  hashes     : Array (Lean.Name × Option UInt64)
  /-- Names of implementations whose hash at this param differed
      from the baseline (the first entry in `hashes`). Today's
      agreement check is baseline-pivoted so this is exactly what's
      computed; if a future check moves to all-pairwise comparison
      the format would extend to a richer relation. Functions whose
      hash was absent at this param do NOT appear here — that's
      data missing, not data disagreeing. -/
  dissenters : Array Lean.Name
  deriving Inhabited, Repr

/-- Whether the implementations in a `compare` agree on shared
params. The `divergedAt` payload preserves first-divergence order:
`details[0]` is the earliest common param where any disagreement was
observed, so reports can highlight it as the place to start
debugging. -/
inductive AgreementStatus
  | allAgreed
  /-- One entry per diverging param. The first entry is the earliest
      common param (in `commonParams` order) where any pair of
      implementations disagreed. -/
  | divergedAt (details : Array DivergenceDetail)
  /-- Hash agreement could not be checked because at least one
      compared function's return type lacks a `Hashable` instance.
      The payload names the offending functions so the user knows
      which registration to fix. -/
  | hashUnavailable (unhashed : Array Lean.Name)
  deriving Repr, Inhabited

structure ComparisonReport where
  results       : Array BenchmarkResult
  /-- Intersection of every compared function's measured params. -/
  commonParams  : Array Nat
  agreeOnCommon : AgreementStatus
  deriving Inhabited

/-! ## Fixed-problem benchmarks

A fixed benchmark records the wall-clock time for a single canonical
input — there is no parameter to walk and no complexity model to
verify. The user registers a value of type `α` or `IO α` (the
benchmark name is the function name); the harness records `repeats`
measured invocations and reports median/min/max plus a result-hash
agreement check.

The type / runtime split mirrors the parametric path: a `FixedSpec`
record (compile-time persistent extension) plus a runtime closure
in a separate registry, both populated from a single
`setup_fixed_benchmark` macro emission.
-/

/-- Per-benchmark configuration for a fixed-problem benchmark.

Defaults are tuned for the "single hard problem ~1s" use case: a
warmup call followed by 5 measured calls, with a 60s wallclock cap
per call (much higher than the parametric default of 1s, since the
whole point of a fixed benchmark is recording the absolute time of
something genuinely expensive).

Override defaults at declaration time via
`setup_fixed_benchmark name where { … }` or at run time via flags
on `run` / `compare` (e.g. `--repeats 10 --max-seconds-per-call 30`). -/
structure FixedBenchmarkConfig where
  /-- Number of measured invocations per `run`. The harness performs
      one warmup call before this count, which is discarded. -/
  repeats           : Nat   := 5
  /-- Hard wallclock cap for any single invocation (s). Parent kills
      child past this. -/
  maxSecondsPerCall : Float := 60.0
  /-- Grace ms between SIGTERM and SIGKILL on the child. -/
  killGraceMs       : Nat   := 100
  /-- Whether to perform a single discarded warmup call before the
      measured calls. Defaults to true. -/
  warmup            : Bool  := true
  /-- User-defined tags for organizing benchmarks. Same semantics as
      `BenchmarkConfig.tags` — see issue #10. -/
  tags              : Array String := #[]
  deriving Inhabited, Repr, BEq

/-- Run-time overrides applied on top of a benchmark's declared
`FixedBenchmarkConfig`. Each `none` field leaves the declared value
untouched; each `some` replaces it. -/
structure FixedConfigOverride where
  repeats?           : Option Nat   := none
  maxSecondsPerCall? : Option Float := none
  killGraceMs?       : Option Nat   := none
  warmup?            : Option Bool  := none
  deriving Inhabited, Repr

/-- Apply overrides on top of a fixed config. `none` keeps the value. -/
def FixedConfigOverride.apply (o : FixedConfigOverride)
    (c : FixedBenchmarkConfig) : FixedBenchmarkConfig :=
  { c with
    repeats           := o.repeats?.getD           c.repeats
    maxSecondsPerCall := o.maxSecondsPerCall?.getD c.maxSecondsPerCall
    killGraceMs       := o.killGraceMs?.getD       c.killGraceMs
    warmup            := o.warmup?.getD            c.warmup }

def FixedBenchmarkConfig.validate (c : FixedBenchmarkConfig) : Except String Unit := do
  unless c.maxSecondsPerCall > 0.0 do
    .error s!"FixedBenchmarkConfig.maxSecondsPerCall must be > 0; got {c.maxSecondsPerCall}"
  unless c.repeats ≥ 1 do
    .error s!"FixedBenchmarkConfig.repeats must be ≥ 1; got {c.repeats}"
  pure ()

/-- One entry in the fixed-benchmark registry. -/
structure FixedSpec where
  /-- The registered value. -/
  name              : Lean.Name
  /-- Auto-generated runner that performs one timed invocation and
      returns `(totalNanos, resultHash?)`. -/
  runnerName        : Lean.Name
  /-- Auto-generated `FixedBenchmarkConfig` def carrying any
      `where { ... }` overrides applied at declaration time. -/
  configDeclName    : Lean.Name := Lean.Name.anonymous
  /-- True iff the registered value's underlying type has a
      `Hashable` instance. -/
  hashable          : Bool
  config            : FixedBenchmarkConfig
  deriving Inhabited, Repr

/-- One repeat of a fixed-benchmark run. -/
structure FixedDataPoint where
  /-- 0-based index across the `repeats` measured calls. The single
      warmup call is not recorded. -/
  repeatIndex : Nat
  totalNanos  : Nat
  /-- Present iff the benchmark's value type has `Hashable`. -/
  resultHash  : Option UInt64
  status      : Status
  /-- Memory metrics — same semantics as on `DataPoint`, captured at
      the end of the child invocation. Issue #6. -/
  allocBytes  : Option Nat := none
  peakRssKb   : Option Nat := none
  deriving Repr, Inhabited

/-- Result of a single fixed-benchmark run. -/
structure FixedResult where
  function   : Lean.Name
  /-- True iff the registered value's underlying type has a
      `Hashable` instance — mirrored from `FixedSpec.hashable`. Same
      role as on `BenchmarkResult`: distinguishes "no Hashable" from
      "no successful repeat" in `compare` reporting. -/
  hashable   : Bool := false
  config     : FixedBenchmarkConfig
  points     : Array FixedDataPoint
  /-- Median wall time across `ok` repeats (ns). `none` if no
      repeat returned `ok`. -/
  medianNanos? : Option Nat
  /-- Minimum wall time across `ok` repeats (ns). -/
  minNanos?    : Option Nat
  /-- Maximum wall time across `ok` repeats (ns). -/
  maxNanos?    : Option Nat
  /-- True iff every `ok` repeat produced the same hash (or hashing
      was unavailable across the board). False iff repeats disagreed
      — that's a non-determinism bug in the registered function. -/
  hashesAgree  : Bool
  /-- Reproducibility metadata captured at parent run start (issue
      #11). Same semantics as on `BenchmarkResult`. -/
  env? : Option Env := none
  /-- True when the run was cut short by a CI-budget deadline
      (`--total-seconds`, issue #9). Recorded `points` reflect the
      repeats that completed before the deadline. Default `false`. -/
  budgetTruncated : Bool := false
  deriving Inhabited, Repr

/-- A single fixed-benchmark divergence record: the per-function
hash table (one entry per compared function, CLI argument order)
plus the explicit list of disagreeing pairs against the first
implementation. Mirrors `DivergenceDetail` minus the `param` field —
fixed benchmarks have no parameter sweep. -/
structure FixedDivergenceDetail where
  /-- One entry per compared function, in CLI argument order. The
      hash is the function's *first* `ok` repeat hash (intra-function
      consistency across repeats is recorded separately on
      `FixedResult.hashesAgree`). `none` if the function produced no
      `ok` repeat with a hash. The first entry is the comparison
      baseline. -/
  hashes     : Array (Lean.Name × Option UInt64)
  /-- Names of implementations whose first-`ok` hash differed from
      the baseline. -/
  dissenters : Array Lean.Name
  deriving Inhabited, Repr

/-- Whether the implementations in a fixed `compare` agree on
output. -/
inductive FixedAgreementStatus
  | allAgreed
  | diverged (detail : FixedDivergenceDetail)
  /-- At least one function's return type lacks `Hashable`. The
      payload names the offending functions. -/
  | hashUnavailable (unhashed : Array Lean.Name)
  deriving Repr, Inhabited

structure FixedComparisonReport where
  results       : Array FixedResult
  agreeOnHash   : FixedAgreementStatus
  deriving Inhabited

end LeanBench
