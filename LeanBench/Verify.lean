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

/-- Verify the named benchmarks (or all of them, if `names` is empty).

    Throws `IO.userError` for any name that isn't registered, so the
    caller can surface the failure cleanly to the user. -/
def verify (names : List Lean.Name) : IO (Array VerifyReport) := do
  let entries ← match names with
    | []    => allRuntimeEntries
    | names =>
      let mut acc : Array RuntimeEntry := #[]
      for n in names do
        match ← findRuntimeEntry n with
        | some e => acc := acc.push e
        | none   => throw (.userError s!"unregistered benchmark: {n}")
      pure acc
  entries.mapM (fun e => verifyOne e.spec)

end LeanBench
