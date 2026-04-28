import Lean
import LeanBench.Core
import LeanBench.Env
import LeanBench.Run

/-!
# `LeanBench.Verify` — registration sanity checks

`lean-bench verify` is a smoke test for registered benchmarks. For
each benchmark it spawns the same child-process dispatch that `run`
uses, but at small parameter values (`f 0` and `f 1`) and with a
tight inner-tuning target. The aim is to surface broken registrations
(child fails to start, output is unparseable, hashing path crashes,
function panics at small inputs) before a user invests in a full
`run`.

Checks performed per benchmark:

1. The child process starts and emits a row that the parent can parse
   (covered by `runOneBatch` itself returning a non-error `DataPoint`).
2. The child reports `status: "ok"` for `param = 0` and `param = 1`.
3. If the benchmark's return type has a `Hashable` instance, the
   child emitted a `result_hash`.

Each check is bounded by the spec's existing `maxSecondsPerCall` cap,
so a misbehaving benchmark cannot hang `verify` indefinitely.
-/

open Lean

namespace LeanBench

/-- Tight inner-tuning target used by `verify`. We only need enough
    work to amortise child startup and exercise the hashing path; we
    don't need a stable measurement. 1ms (so `autoTune` doubles until
    ≥500µs) is well below the default 500ms used by `run`. -/
def verifyTargetInnerNanos : Nat := 1_000_000

/-- The params we exercise per benchmark. -/
def verifyParams : Array Nat := #[0, 1]

/-- One check inside a `VerifyReport`. -/
structure VerifyCheck where
  /-- The parameter the child was invoked with. -/
  param   : Nat
  /-- The `DataPoint` the parent observed. -/
  point   : DataPoint
  /-- `none` iff the check passed. Otherwise a short human-readable
      description of why it failed. -/
  failure : Option String
  deriving Inhabited

def VerifyCheck.passed (c : VerifyCheck) : Bool := c.failure.isNone

/-- The verification result for a single benchmark. -/
structure VerifyReport where
  spec   : BenchmarkSpec
  checks : Array VerifyCheck
  deriving Inhabited

def VerifyReport.passed (r : VerifyReport) : Bool :=
  r.checks.all (·.passed)

/-- Decide whether one `(param, DataPoint)` observation passes the
    sanity checks for `spec`. -/
def classifyCheck (spec : BenchmarkSpec) (param : Nat) (dp : DataPoint) :
    Option String :=
  match dp.status with
  | .ok =>
    if spec.hashable && dp.resultHash.isNone then
      some s!"hashable benchmark but child returned no result_hash at param={param}"
    else
      none
  | .timedOut    => some s!"timed out at param={param}"
  | .killedAtCap => some s!"killed at maxSecondsPerCall cap at param={param}"
  | .error msg   => some s!"child error at param={param}: {msg}"

/-- Verify one registered benchmark. -/
def verifyOne (spec : BenchmarkSpec) : IO VerifyReport := do
  -- Use the spec's own cap so users can opt into more time per call,
  -- but shrink the inner-tuning target so verify is cheap.
  let liteCfg := { spec.config with targetInnerNanos := verifyTargetInnerNanos }
  let liteSpec := { spec with config := liteCfg }
  let mut checks : Array VerifyCheck := #[]
  for param in verifyParams do
    let dp ← runOneBatch liteSpec param
    let failure := classifyCheck spec param dp
    checks := checks.push { param, point := dp, failure }
  return { spec, checks }

/-! ## Fixed-benchmark verification

A single one-shot child invocation per fixed benchmark, with the
spec's own kill cap but the standard cheap warmup-off shape. Same
classification logic as the parametric path but specialised to
fixed-benchmark data. -/

/-- One check inside a `FixedVerifyReport`. -/
structure FixedVerifyCheck where
  point   : FixedDataPoint
  failure : Option String
  deriving Inhabited

def FixedVerifyCheck.passed (c : FixedVerifyCheck) : Bool := c.failure.isNone

structure FixedVerifyReport where
  spec   : FixedSpec
  checks : Array FixedVerifyCheck
  deriving Inhabited

def FixedVerifyReport.passed (r : FixedVerifyReport) : Bool :=
  r.checks.all (·.passed)

def classifyFixedCheck (spec : FixedSpec) (dp : FixedDataPoint) : Option String :=
  match dp.status with
  | .ok =>
    if spec.hashable && dp.resultHash.isNone then
      some "hashable fixed benchmark but child returned no result_hash"
    else
      none
  | .timedOut    => some "timed out on warmup invocation"
  | .killedAtCap => some "killed at maxSecondsPerCall cap on warmup invocation"
  | .error msg   => some s!"child error: {msg}"

/-- Verify one registered fixed benchmark by spawning a single
    one-shot child run. The spec's `maxSecondsPerCall` cap applies. -/
def verifyFixedOne (spec : FixedSpec) : IO FixedVerifyReport := do
  let dp ← runFixedOneBatch spec spec.config 0
  let failure := classifyFixedCheck spec dp
  return { spec, checks := #[{ point := dp, failure }] }

/-! ## Combined verify across both registries -/

/-- Result of a `verify` run: parametric reports first, then fixed
    reports. Either array can be empty. -/
structure CombinedVerifyReports where
  parametric : Array VerifyReport
  fixed      : Array FixedVerifyReport
  deriving Inhabited

def CombinedVerifyReports.passed (r : CombinedVerifyReports) : Bool :=
  r.parametric.all (·.passed) && r.fixed.all (·.passed)

def CombinedVerifyReports.totalCount (r : CombinedVerifyReports) : Nat :=
  r.parametric.size + r.fixed.size

def CombinedVerifyReports.failedCount (r : CombinedVerifyReports) : Nat :=
  (r.parametric.filter (! ·.passed)).size +
  (r.fixed.filter (! ·.passed)).size

/-- Verify the named benchmarks (or all of them, if `names` is empty).
    Resolves each name against both the parametric and fixed
    registries; an unregistered name throws.

    Throws `IO.userError` for any name that isn't registered, so the
    caller can surface the failure cleanly to the user. -/
def verify (names : List Lean.Name) : IO CombinedVerifyReports := do
  match names with
  | [] =>
    let pEntries ← allRuntimeEntries
    let fEntries ← allFixedRuntimeEntries
    let parametric ← pEntries.mapM fun e => verifyOne e.spec
    let fixed ← fEntries.mapM fun e => verifyFixedOne e.spec
    return { parametric, fixed }
  | _ =>
    let mut parametric : Array VerifyReport := #[]
    let mut fixed : Array FixedVerifyReport := #[]
    for n in names do
      match ← findRuntimeEntry n with
      | some e => parametric := parametric.push (← verifyOne e.spec)
      | none =>
        match ← findFixedRuntimeEntry n with
        | some e => fixed := fixed.push (← verifyFixedOne e.spec)
        | none => throw (.userError s!"unregistered benchmark: {n}")
    return { parametric, fixed }

end LeanBench
