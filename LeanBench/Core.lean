import Lean.Data.Name

/-!
# `LeanBench.Core` — shared types

Defined here so `Setup`, `Run`, `Child`, `Stats`, `Compare`, and
`Format` can all import a single ground-truth source for the data
shapes. No runtime behaviour in this file.
-/

open Lean (Name)

namespace LeanBench

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

    `.auto` is the default — at runtime it inspects the declared
    complexity and resolves to one of the above. The heuristic
    compares `complexity(2n)/complexity(n)` at two scales: for any
    `n^k` it is constant (→ doubling); for any `b^n` it grows
    geometrically (→ linear). -/
inductive ParamSchedule
  | doubling
  | linear (samples : Nat := 16)
  | auto
  deriving Inhabited, Repr

/-- Per-benchmark configuration. Defaults are the v0.1 contract. -/
structure BenchmarkConfig where
  /-- Target wall time per inner-tuned batch (ns). Default 500ms. -/
  targetInnerNanos  : Nat   := 500_000_000
  /-- Hard wallclock cap for any single batch (s). Parent kills child past this. -/
  maxSecondsPerCall : Nat   := 1
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
  deriving Inhabited, Repr

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
  /-- True iff the function's return type has a `Hashable` instance;
      hashing is enabled in `runChecked` only when this is set. -/
  hashable         : Bool
  config           : BenchmarkConfig
  deriving Inhabited, Repr

/-- `(param, C = perCallNanos / complexity param)`. Source of truth
  for the verdict. -/
abbrev Ratio := Nat × Float

/-- Result of a single benchmark invocation. -/
structure BenchmarkResult where
  function   : Lean.Name
  /-- The expected-complexity formula the user declared, as printable
      source (e.g. `"2 ^ n"`). Echoed back in the result header so
      reports stay readable without needing the env around. -/
  complexityFormula : String
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
  deriving Inhabited, Repr

/-- Whether two functions in a `compare` agree on shared params. -/
inductive AgreementStatus
  | allAgreed
  | divergedAt (entries : Array (Lean.Name × Lean.Name × Nat))
  | hashUnavailable
  deriving Repr, Inhabited

structure ComparisonReport where
  results       : Array BenchmarkResult
  /-- Intersection of every compared function's measured params. -/
  commonParams  : Array Nat
  agreeOnCommon : AgreementStatus
  deriving Inhabited

end LeanBench
