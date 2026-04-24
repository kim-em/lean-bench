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
  deriving Repr, Inhabited

/-- Heuristic verdict — see SPEC v0.1: weak labels by design. -/
inductive Verdict
  | consistentWithDeclaredComplexity
  | inconclusive
  deriving Repr, Inhabited, BEq

def Verdict.toString : Verdict → String
  | .consistentWithDeclaredComplexity => "consistentWithDeclaredComplexity"
  | .inconclusive => "inconclusive"

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
  deriving Inhabited, Repr

/-- One entry in the benchmark registry. Stored as `Name`s only;
neither `Syntax` nor `Expr`, sidestepping serialization issues. -/
structure BenchmarkSpec where
  /-- The function under test. -/
  name             : Lean.Name
  /-- Auto-generated `Nat → Nat` complexity model. -/
  complexityName   : Lean.Name
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
  complexity : Name
  config     : BenchmarkConfig
  points     : Array DataPoint
  ratios     : Array Ratio
  verdict    : Verdict
  cMin?      : Option Float
  cMax?      : Option Float
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
