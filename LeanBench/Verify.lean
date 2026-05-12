import Lean
import LeanBench.Core
import LeanBench.Env

/-!
# `LeanBench.Verify` — registration sanity checks

`lean-bench verify` is a smoke gate for registered benchmarks. For
each benchmark it invokes the registered runner **in-process** at
small parameter values (`f 0` and `f 1` for parametric specs;
`count = 1` for fixed specs) and reports any non-`ok` outcome. The
aim is to surface broken registrations (function panics at small
inputs, hashing path crashes, missing `Hashable` wiring) before a
user invests in a full `run`.

Checks performed per parametric benchmark:

1. The registry has an entry for the spec's name. A missing entry
   means the bench-exe was built without the registration's
   `setup_benchmark` site, or with a stale registry.
2. The registered runner returns without throwing an `IO.Error` at
   `param = 0` and `param = 1`.
3. If the benchmark's return type has a `Hashable` instance, the
   runner emits a `result_hash`.

## Isolation tradeoffs (read before "fixing" this)

Earlier versions of `verify` spawned a fresh child process for each
`(spec, param)` pair and reused the full `runOneBatch` pipeline.
That gave OS-level panic + hang isolation at the cost of `O(specs ×
params)` process startups; on libraries with dozens of registrations
the startup cost dominated CI time (~9 s/spawn × 40 spawns for a
single bench-exe).

The in-process path trades that isolation for one process startup
per `lake exe X_bench verify`. The tradeoffs are accepted, not
papered over:

* **Panic isolation.** A Lean-level exception from the runner is
  caught by the `try ... catch` below and surfaced as a
  `VerifyCheck.failure`. Native aborts from `@[extern]` C code
  (e.g. `panic_fn`, C `assert` failures) are **not catchable**
  and will crash the bench-exe. In practice the verify-time
  parameters (0, 1) are tiny and rarely exercise extern paths
  heavily; if one of your benches aborts natively at param=1,
  fix the bench (it's a real bug) or add a child-spawn fallback
  for that specific registration.
* **Hang isolation.** A runner that loops forever blocks the
  bench-exe until the outer CI step timeout fires. There is no
  `maxSecondsPerCall` kill on the in-process path. Do **not**
  wrap calls in `IO.asTask + timer` to pretend otherwise: Lean's
  async cancellation is cooperative and cannot interrupt a
  CPU-bound closure; a fake timeout would "return" while leaving
  a runaway task consuming CPU. If a verify-time hang surfaces,
  fix the bench — verify inputs are 0 and 1.

The `run` path is untouched and continues to enforce
process-level isolation and `maxSecondsPerCall` via `runOneBatch`.
-/

open Lean

namespace LeanBench

/-- The params we exercise per parametric benchmark. -/
private def verifyParams : Array Nat := #[0, 1]

/-- One check inside a `VerifyReport`. -/
structure VerifyCheck where
  /-- The parameter the runner was invoked with. -/
  param   : Nat
  /-- The `DataPoint` synthesised from the in-process invocation. -/
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
      some s!"hashable benchmark but runner returned no result_hash at param={param}"
    else
      none
  | .timedOut    => some s!"timed out at param={param}"
  | .killedAtCap => some s!"killed at maxSecondsPerCall cap at param={param}"
  | .error msg   => some s!"runner error at param={param}: {msg}"

/-- Build a `.ok` DataPoint for an in-process check. Smoke rows are
    not verdict-eligible. -/
private def okDataPoint (param : Nat) (loopNanos : Nat)
    (hash : Option UInt64) : DataPoint :=
  { param
  , innerRepeats := 1
  , totalNanos := loopNanos
  , perCallNanos := loopNanos.toFloat
  , resultHash := hash
  , status := .ok
  , partOfVerdict := false }

/-- Build a `.error` DataPoint for an in-process check whose runner
    threw a Lean-level exception. -/
private def errorDataPoint (param : Nat) (msg : String) : DataPoint :=
  { param
  , innerRepeats := 0
  , totalNanos := 0
  , perCallNanos := 0.0
  , resultHash := none
  , status := .error msg
  , partOfVerdict := false }

/-- Verify one registered benchmark **in-process**. Looks up the
    runner in the runtime registry, invokes it once per
    `verifyParams` entry inside `try ... catch`, and classifies the
    outcome. A missing registry entry produces a single failed check
    so the report still flows through `classifyCheck`'s normal path. -/
def verifyOne (spec : BenchmarkSpec) : IO VerifyReport := do
  match ← findRuntimeEntry spec.name with
  | none =>
    let msg := s!"unregistered benchmark: {spec.name}"
    let dp := errorDataPoint 0 msg
    let failure := classifyCheck spec 0 dp
    return { spec, checks := #[{ param := 0, point := dp, failure }] }
  | some entry =>
    let mut checks : Array VerifyCheck := #[]
    for param in verifyParams do
      let dp ← try
        let inner ← entry.runner param
        let (loopNanos, hash) ← inner 1
        pure (okDataPoint param loopNanos hash)
      catch e =>
        pure (errorDataPoint param e.toString)
      let failure := classifyCheck spec param dp
      checks := checks.push { param, point := dp, failure }
    return { spec, checks }

/-! ## Fixed-benchmark verification

A single in-process invocation per fixed benchmark, count = 1. Same
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
      some "hashable fixed benchmark but runner returned no result_hash"
    else
      -- Smoke check only: a single in-process call with no warmup,
      -- so a pass here doesn't guarantee `run` will pass
      -- (warmup-sensitive or multi-repeat-disagreement cases can
      -- still surface there). Issue #55.
      if !spec.hashable then none
      else match spec.config.expectedHash, dp.resultHash with
        | some expected, some got =>
          if got == expected then none
          else some s!"expectedHash mismatch — declared 0x{String.ofList (Nat.toDigits 16 expected.toNat)}, got 0x{String.ofList (Nat.toDigits 16 got.toNat)}"
        | _, _ => none
  | .timedOut    => some "timed out on warmup invocation"
  | .killedAtCap => some "killed at maxSecondsPerCall cap on warmup invocation"
  | .error msg   => some s!"runner error: {msg}"

/-- Build a `.ok` FixedDataPoint for an in-process check. -/
private def okFixedDataPoint (loopNanos : Nat) (hash : Option UInt64) :
    FixedDataPoint :=
  { repeatIndex := 0
  , innerRepeats := 1
  , totalNanos := loopNanos
  , resultHash := hash
  , status := .ok }

/-- Build a `.error` FixedDataPoint for an in-process check whose
    runner threw. -/
private def errorFixedDataPoint (msg : String) : FixedDataPoint :=
  { repeatIndex := 0
  , innerRepeats := 0
  , totalNanos := 0
  , resultHash := none
  , status := .error msg }

/-- Verify one registered fixed benchmark by invoking its runner
    in-process with `count = 1`. -/
def verifyFixedOne (spec : FixedSpec) : IO FixedVerifyReport := do
  match ← findFixedRuntimeEntry spec.name with
  | none =>
    let msg := s!"unregistered fixed benchmark: {spec.name}"
    let dp := errorFixedDataPoint msg
    let failure := classifyFixedCheck spec dp
    return { spec, checks := #[{ point := dp, failure }] }
  | some entry =>
    let dp ← try
      let (loopNanos, hash) ← entry.runner 1
      pure (okFixedDataPoint loopNanos hash)
    catch e =>
      pure (errorFixedDataPoint e.toString)
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
