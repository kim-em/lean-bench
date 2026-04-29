import Lean
import Std.Sync.Mutex
import LeanBench.Core
import LeanBench.Env
import LeanBench.RunEnv
import LeanBench.Schema
import LeanBench.Stats

/-!
# `LeanBench.Run` — parent-side orchestration

For each param in the chosen ladder shape (`ParamSchedule`), spawn
the same binary as a child:

1. Spawn the same binary as a child (`_child --bench NAME --param N
   --target-nanos T`) directly via `IO.Process.spawn`, and start a
   sibling `Task` that sleeps for `maxSecondsPerCall + killGraceMs`
   and then calls `IO.Process.Child.kill` if the child is still
   running. No external `timeout(1)` dependency: pure-Lean,
   cross-platform (works on every platform Lean's process API
   supports).
2. Read the child's stdout (one JSONL row).
3. If the child exited 0 with a parseable row → use it.
   If the killer task fired → synthesize a `killedAtCap` row.
   If the child died otherwise → synthesize an `error` row.
4. Hand the points to `Stats.summarize` for ratio + verdict.

Two ladder shapes:

- **Doubling** (`.doubling`): walk `paramFloor, 1, 2, 4, …` up to
  `paramCeiling`. Right for polynomial complexity.
- **Linear** (`.linear samples`): doubling probe brackets the
  productive band `(lastOk, firstFail)`, then `samples` rungs are
  evenly spaced strictly inside that bracket. Probe rows get
  `partOfVerdict := false` so they don't corrupt `cMin/cMax`/slope;
  sweep rows get `partOfVerdict := true`. Right for exponential
  complexity, where the cap kills doubling within 1–2 useful rungs.

`.auto` (the default) inspects the declared complexity at runtime
and resolves to one of the above by comparing `complexity(64) /
complexity(32)` against `complexity(16) / complexity(8)`: for any
`n^k` they're equal (→ doubling); for any `b^n` the former dominates
(→ linear).
-/

open Lean

namespace LeanBench

/-- Path of the running binary, so we can spawn ourselves as the child. -/
def ownExe : IO String := do
  return (← IO.appPath).toString

/-! ## CI-budget deadline (issue #9)

A `BudgetDeadline` is an absolute monotonic-clock instant past which
the budgeted-run orchestrator stops scheduling new work. The deadline
is plumbed through `runBenchmark` / `runFixedBenchmark` so a single
benchmark can also be cut short between rungs (rather than only at
benchmark boundaries). Each child spawn is still independently capped
by `maxSecondsPerCall`, so the worst-case bound on total wall time is
approximately `total_seconds + maxSecondsPerCall` (one rung may have
been in flight when the deadline was checked). See
[`doc/quickstart.md#ci-budget-mode`](../doc/quickstart.md#ci-budget-mode). -/
abbrev BudgetDeadline := Nat

/-- Has the deadline passed? `none` (no budget) is always `false`. -/
def deadlineExceeded? (deadline? : Option BudgetDeadline) : IO Bool := do
  match deadline? with
  | none => return false
  | some d =>
    let now ← IO.monoNanosNow
    return now ≥ d

/-- Parse a hex-prefixed UInt64 string like `"0xdeadbeef"`. -/
private def parseHexU64 (s : String) : Option UInt64 := do
  guard (s.startsWith "0x")
  let body := s.drop 2
  let n := body.foldl (init := 0) fun acc c =>
    let d :=
      if c.isDigit then c.toNat - '0'.toNat
      else if 'a'.toNat ≤ c.toNat ∧ c.toNat ≤ 'f'.toNat then c.toNat - 'a'.toNat + 10
      else if 'A'.toNat ≤ c.toNat ∧ c.toNat ≤ 'F'.toNat then c.toNat - 'A'.toNat + 10
      else 0
    acc * 16 + d
  return n.toUInt64

/-- Parse the child's JSONL row into a `DataPoint`.

Schema-compatibility contract (see [`doc/schema.md`](../../doc/schema.md)):

- `schema_version > Schema.schemaVersion` is rejected with an
  explicit error. A missing `schema_version` is tolerated for
  back-compat and treated as v1.
- Required fields (`param`, `inner_repeats`, `total_nanos`, `status`)
  are still required; absence is a parse error.
- Optional fields (`per_call_nanos`, `result_hash`, `error`) are
  tolerated when missing or `null`.
- Unknown extra keys are ignored — that's the forward-compat lever
  that lets future PRs add fields (memory metrics, env metadata,
  budget tags, …) without rewriting every reader. -/
def parseChildRow (line : String) : Except String DataPoint := do
  let json ← Json.parse line
  Schema.checkVersion json
  Schema.checkKind Schema.kindParametric json
  -- `function` is required by the schema. The parent already knows
  -- what it dispatched, but external tooling reading raw JSONL needs
  -- this as a sanity check, so the parser still requires its
  -- presence.
  let _ ← json.getObjValAs? String "function"
  let param ← json.getObjValAs? Nat "param"
  let innerRepeats ← json.getObjValAs? Nat "inner_repeats"
  let totalNanos ← json.getObjValAs? Nat "total_nanos"
  let perCallNanos : Float :=
    match json.getObjValAs? Float "per_call_nanos" with
    | .ok x => x
    | .error _ => totalNanos.toFloat / (max 1 innerRepeats).toFloat
  let resultHash : Option UInt64 :=
    match json.getObjVal? "result_hash" with
    | .ok (.str s) => parseHexU64 s
    | _ => none
  let statusStr ← json.getObjValAs? String "status"
  let status : Status := match statusStr with
    | "ok" => .ok
    | "timed_out" => .timedOut
    | "killed_at_cap" => .killedAtCap
    | "error" =>
        .error ((json.getObjValAs? String "error").toOption.getD "")
    | _ => .error s!"unknown status: {statusStr}"
  -- Memory metrics (issue #6) — both optional, both may be JSON `null`
  -- on platforms that can't supply them. A type mismatch or an
  -- explicit `null` collapses to `none`; absence likewise.
  let allocBytes : Option Nat :=
    match json.getObjValAs? Nat "alloc_bytes" with
    | .ok n => some n
    | .error _ => none
  let peakRssKb : Option Nat :=
    match json.getObjValAs? Nat "peak_rss_kb" with
    | .ok n => some n
    | .error _ => none
  return {
    param, innerRepeats, totalNanos, perCallNanos,
    resultHash, status, allocBytes, peakRssKb }

/-- Synthesize a row when the child died before emitting. -/
def synthRow (param : Nat) (status : Status) : DataPoint :=
  { param, innerRepeats := 0, totalNanos := 0, perCallNanos := 0.0
  , resultHash := none, status, partOfVerdict := true }

/-- Pure classifier for the post-spawn outcome of a child run. Folds
    `(exit, stdout, stderr, wasKilled)` into a `DataPoint` using exactly
    the same rules `runOneBatch` applies inline. Extracted so the failure
    branches (empty stdout, malformed JSONL, non-zero exit, killed-at-cap)
    are unit-testable without spawning a real subprocess.

    The `stderrText` argument is appended to synthesized error messages so
    the user sees whatever the child wrote before dying. Pass `""` when
    there's nothing to attach. -/
def classifyChildOutcome
    (param : Nat) (exit : UInt32) (stdout stderrText : String)
    (wasKilled : Bool) : DataPoint :=
  let withStderr (msg : String) : String :=
    if stderrText.isEmpty then msg else s!"{msg}; stderr: {stderrText}"
  if wasKilled then
    synthRow param .killedAtCap
  else match exit with
    | 0 =>
      let line := stdout.trimAscii.toString
      if line.isEmpty then
        synthRow param (.error (withStderr "child exited 0 but produced no output"))
      else match parseChildRow line with
        | .ok row => row
        | .error e => synthRow param (.error (withStderr s!"parse: {e}"))
    | other =>
      synthRow param (.error (withStderr s!"child exited with code {other}"))

/-- Spawn one child invocation and return the resulting DataPoint.

    The `env?` argument is the parent's pre-captured environment
    snapshot (issue #11); when `some`, it's serialized via
    `--env-json` so the child stamps the same env on its row instead
    of re-shelling-out to `git` / `hostname` / `/proc/cpuinfo` per
    rung. When `none`, the child captures fresh — used by helper
    paths (`measureSpawnFloor`, `verifyOne`) where threading env
    through isn't worth the plumbing. -/
def runOneBatch (spec : BenchmarkSpec) (param : Nat) (env? : Option Env := none) :
    IO DataPoint := do
  let exe ← ownExe
  let target := spec.config.targetInnerNanos
  let deadlineMs : UInt32 :=
    (spec.config.maxSecondsPerCall * 1000.0 + spec.config.killGraceMs.toFloat).toUInt32
  -- The cache-mode flag is passed positionally (`--cache-mode warm` /
  -- `cold`) so the child knows which timing strategy to use. The
  -- existing `--target-nanos` is unused in cold mode but still passed
  -- for parser symmetry.
  let baseArgs : Array String := #[
    "_child",
    "--bench", spec.name.toString (escape := false),
    "--param", toString param,
    "--target-nanos", toString target,
    "--cache-mode", spec.config.cacheMode.toJsonString
  ]
  let args : Array String :=
    match env? with
    | some env => baseArgs ++ #["--env-json", (RunEnv.toJson env).compress]
    | none     => baseArgs
  let child ← IO.Process.spawn {
    cmd := exe
    args := args
    stdout := .piped
    stderr := .piped
    stdin := .null
  }
  -- Drain stderr concurrently so a chatty child can't fill the pipe
  -- buffer and deadlock its own write — the parent would then
  -- misreport the child as timing out / killed at cap. Whatever the
  -- child wrote to stderr is appended to the synthesized error row
  -- when the child exits non-zero.
  let stderrTask ← child.stderr.readToEnd.asTask
  -- Two flags + a mutex coordinate the parent and the killer task:
  --
  --   * `mainDoneRef` flips to `true` once the parent has finished
  --     reading stdout and is about to call `child.wait` (which on
  --     POSIX reaps the pid).
  --   * `killedRef` flips to `true` iff the killer issued the kill.
  --
  -- The killer takes the lock, reads `mainDoneRef`, and only kills if
  -- the parent hasn't reached the post-stdout point yet. The parent
  -- takes the lock to set `mainDoneRef` *before* calling
  -- `child.wait`. This ordering guarantees we never call `kill(2)` on
  -- a pid that has already been reaped — a reaped pid can be reused
  -- by the kernel, so signalling it would risk hitting an unrelated
  -- process.
  --
  -- The killer polls `mainDoneRef` while sleeping so it terminates
  -- quickly when the child finishes early; otherwise every batch
  -- would block here for the full `deadlineMs`.
  let mainDoneRef ← IO.mkRef false
  let killedRef ← IO.mkRef false
  let mutex ← Std.BaseMutex.new
  let _killer ← IO.asTask do
    let pollMs : UInt32 := 25
    let deadline : Nat := deadlineMs.toNat
    let mut waited : Nat := 0
    let mut bailed := false
    while waited < deadline && !bailed do
      if (← mainDoneRef.get) then
        bailed := true
      else
        let step : Nat := min pollMs.toNat (deadline - waited)
        IO.sleep step.toUInt32
        waited := waited + step
    unless bailed do
      mutex.lock
      unless (← mainDoneRef.get) do
        killedRef.set true
        (try child.kill catch _ => pure ())
      mutex.unlock
  let stdout ← child.stdout.readToEnd
  mutex.lock
  mainDoneRef.set true
  mutex.unlock
  let exit ← child.wait
  let stderrText : String :=
    match stderrTask.get with
    | .ok s => s.trimAscii.toString
    | .error _ => ""
  -- The killer writes `killedRef := true` *before* it calls `Child.kill`,
  -- so by the time `child.wait` has returned (which can only happen
  -- after the kill takes effect, in the kill case) the flag is already
  -- committed. Hence we don't have to synchronize with the killer task.
  let wasKilled ← killedRef.get
  return classifyChildOutcome param exit stdout stderrText wasKilled

/-- Doubling ladder from floor through ceiling. -/
def paramLadder (cfg : BenchmarkConfig) : Array Nat := Id.run do
  let mut acc : Array Nat := #[]
  let mut p := cfg.paramFloor
  acc := acc.push p
  if p == 0 then
    p := 1
    acc := acc.push p
  while p ≤ cfg.paramCeiling do
    p := p * 2
    if p ≤ cfg.paramCeiling then
      acc := acc.push p
  return acc

/-- Doubling-rate-of-doubling-rate test for "is this complexity
    super-polynomial?" Compares `complexity(64)/complexity(32)`
    against `complexity(16)/complexity(8)`: equal for any `n^k`,
    grows geometrically for any `b^n` with `b > 1`. The threshold
    of 4× catches even mild exponentials like `(1.1)^n` while
    leaving high-degree polynomials and slow growers on doubling.

    Denominators are clamped to `≥ 1` so that complexity functions
    that bottom out at 0 don't divide by zero. -/
private def autoPicksLinear (complexity : Nat → Nat) : Bool :=
  let c8  := (max 1 (complexity 8)).toFloat
  let c16 := (complexity 16).toFloat
  let c32 := (max 1 (complexity 32)).toFloat
  let c64 := (complexity 64).toFloat
  let r8  := c16 / c8
  let r32 := c64 / c32
  r32 ≥ 4.0 * r8

/-- Estimate the largest `n` past `lastOk` whose run cost should fit
    within `capNanos`, extrapolating from `lastOk`'s observed per-call
    cost via the declared complexity model:

    ```
    predicted(n) ≈ lastOkPerCallNanos × complexity(n) / complexity(lastOk)
    ```

    Returns the first `n ∈ (lastOk, probedFirstFail)` where
    `predicted(n) > safety × capNanos`, or `probedFirstFail` if none.
    The doubling probe's `firstFail` overshoots the real boundary by
    up to a factor of two (cost doubles per `n` doubling for any
    polynomial; far more for an exponential), so this refinement
    matters: without it, an exponential benchmark wastes most of its
    sweep budget on rungs guaranteed to hit the cap. -/
private def estimateFirstFail (complexity : Nat → Nat)
    (lastOk : Nat) (lastOkPerCallNanos : Float)
    (capNanos : Float) (probedFirstFail : Nat) : Nat :=
  let safety : Float := 0.7
  let lastOkComp := (max 1 (complexity lastOk)).toFloat
  let perUnit := lastOkPerCallNanos / lastOkComp
  Id.run do
    let mut n := lastOk + 1
    while n < probedFirstFail do
      if perUnit * (complexity n).toFloat > safety * capNanos then return n
      n := n + 1
    return probedFirstFail

/-- Linear sweep rungs strictly inside `(lastOk, firstFail)`, stride 1,
    capped at `samples` rungs from the bottom of the bracket.

    With a complexity-tightened `firstFail` the bracket is narrow enough
    that stride 1 gives the maximum useful samples without wasting runs
    on rungs that would hit the cap. Returns `#[]` when the bracket is
    empty. -/
private def pickLinearRungs (lastOk firstFail samples : Nat) : Array Nat :=
  if firstFail ≤ lastOk + 1 || samples = 0 then #[] else
    let last := min (firstFail - 1) (lastOk + samples)
    (Array.range (last - lastOk)).map (lastOk + 1 + ·)

/-- Run a single rung, taking `trials` independent outer measurements.
    Each call to `runOneBatch` is one fresh child spawn; per-trial
    points are tagged with `trialIndex` 0..trials-1.

    Always runs all `trials` spawns regardless of intermediate
    failures: a transient first-trial timeout shouldn't poison the
    whole rung's data (Codex flagged this as a stability regression
    when `outerTrials > 1`). The ladder-continuation decision is
    made by the caller from `rung.any (·.status == .ok)`, so a
    rung whose first trial flaked but later trials succeeded is
    still treated as `lastOk`-eligible.

    Cost is bounded: each spawn is independently capped by
    `maxSecondsPerCall`, so the worst case at a known-cap rung is
    `trials × maxSecondsPerCall`. Users opted into that cost when
    they bumped `outerTrials`. Issue #4. -/
private def runRungTrials (spec : BenchmarkSpec) (param : Nat)
    (trials : Nat) (env? : Option Env := none) : IO (Array DataPoint) := do
  let mut acc : Array DataPoint := #[]
  let trials := max 1 trials
  for i in [0 : trials] do
    let dp ← runOneBatch spec param env?
    acc := acc.push { dp with trialIndex := i }
  return acc

/-- Did any trial at this rung succeed? Drives the ladder-continuation
    decision: the rung counts as `ok` for `lastOk` / sweep-progress as
    long as at least one of its `outerTrials` measurements landed.
    Issue #4. -/
private def rungAnyOk (rung : Array DataPoint) : Bool :=
  rung.any (fun dp => dp.status == .ok)

/-- Run the static doubling ladder. Returns `(points, lastOk?, firstFail?, budgetTruncated)`:

- `lastOk?` is the largest param whose status was `.ok` (none if no row
  was ok),
- `firstFail?` is the first param after `lastOk` whose status was non-ok
  (none if the ladder ran to ceiling without failing).

In `.linear` mode the doubling probe exists only to find the
productive band — its rows get `partOfVerdict := false` later and
never enter the verdict reduction. So the probe runs **one trial per
rung** regardless of `outerTrials`: there is no statistical value in
spending `outerTrials × probeRungs` spawns on rows the verdict will
discard. Codex flagged this as wasted cost for `--outer-trials 5
--param-schedule linear`. The verdict-eligible sweep rungs (added
later by `runBenchmark`) still run the full `outerTrials` cluster.

In `.doubling` mode every probe rung also enters the verdict, so
running `outerTrials` per rung does matter — but we still only do
one trial here; `runBenchmark` runs the remaining `outerTrials - 1`
trials at every probe rung that survived in a second pass below
(see `topUpDoublingTrials`). That keeps the bracket-finding cost at
`O(ladder)` regardless of `outerTrials` while preserving the median
trial cluster on the rungs that count.

Probe rows are returned with whatever `partOfVerdict` `runOneBatch`
chose (defaults to true); the caller flips them as needed.

The optional `deadline?` (issue #9) lets the probe stop early when a
CI budget has been exhausted; the fourth return component flips to
`true` in that case so the caller can record `budgetTruncated`.
With `deadline? = none` the flag is always `false`. -/
private def runDoublingProbe (spec : BenchmarkSpec) (env : Env)
    (deadline? : Option BudgetDeadline := none) :
    IO (Array DataPoint × Option Nat × Option Nat × Bool) := do
  let ladder := paramLadder spec.config
  let mut points : Array DataPoint := #[]
  let mut lastOk : Option Nat := none
  let mut firstFail : Option Nat := none
  let mut truncated := false
  for param in ladder do
    if firstFail.isSome then break
    if (← deadlineExceeded? deadline?) then
      truncated := true
      break
    -- Probe runs one trial per rung — see the docstring rationale.
    let rung ← runRungTrials spec param 1 (some env)
    points := points ++ rung
    if rungAnyOk rung then lastOk := some param
    else firstFail := some param
  return (points, lastOk, firstFail, truncated)

/-- After the doubling probe, top up the trials at every successful
    probe rung so each verdict-eligible param ends up with
    `outerTrials` measurements. Only called in `.doubling` mode (where
    probe rows ARE the verdict data); in `.linear` mode probe rows are
    demoted to `partOfVerdict := false` and the linear sweep provides
    the trial cluster on its own.

    Trials added here are tagged with `trialIndex` 1..outerTrials-1
    (the probe contributed index 0). If a top-up trial fails, it still
    gets recorded — we want the user to see the flake rather than have
    it silently dropped — but it doesn't change `lastOk`/`firstFail`
    since those were settled by the probe. Issue #4.

    Returns the accumulated points plus a `budgetTruncated` flag —
    `true` iff the deadline cut the top-up loop short before every
    successful probe rung had its full cluster (issue #9). With
    `deadline? = none` the flag is always `false` and behaviour
    matches the pre-#9 helper. -/
private def topUpDoublingTrials (spec : BenchmarkSpec) (env : Env)
    (probePoints : Array DataPoint)
    (deadline? : Option BudgetDeadline := none) :
    IO (Array DataPoint × Bool) := do
  let extraTrials := spec.config.outerTrials - 1
  if extraTrials == 0 then return (probePoints, false)
  -- Distinct ok params in probe order.
  let mut seen : Std.HashSet Nat := {}
  let mut okParams : Array Nat := #[]
  for dp in probePoints do
    if dp.status == .ok && !seen.contains dp.param then
      seen := seen.insert dp.param
      okParams := okParams.push dp.param
  let mut acc := probePoints
  let mut hitDeadline := false
  for p in okParams do
    if hitDeadline then break
    for i in [0 : extraTrials] do
      if (← deadlineExceeded? deadline?) then
        hitDeadline := true
        break
      let dp ← runOneBatch spec p (some env)
      acc := acc.push { dp with trialIndex := i + 1 }
  return (acc, hitDeadline)

/-- Resolve `.auto` to either `.doubling` or a sensible `.linear`.
    `.custom` and the explicit shapes pass through unchanged. -/
private def resolveSchedule (cfg : BenchmarkConfig) (complexity : Nat → Nat) :
    ParamSchedule :=
  match cfg.paramSchedule with
  | .doubling => .doubling
  | .linear samples => .linear samples
  | .custom params => .custom params
  | .auto =>
    if autoPicksLinear complexity then .linear 16 else .doubling

/-- Annotate every point with `belowSignalFloor` based on the
    measured per-spawn floor. A row crosses the threshold when its
    `totalNanos` is below `multiplier × spawnFloor` — small enough
    that subprocess overhead dominates the inner-loop work and the
    measurement is unreliable as algorithm timing.

    `multiplier ≤ 1.0` disables the check (every row is left
    unflagged); `spawnFloorNanos = 0` (or `none`) likewise leaves
    everything unflagged because we have no reference to compare
    against. Issue #15. -/
def annotateBelowSignalFloor (multiplier : Float)
    (spawnFloorNanos? : Option Nat) (points : Array DataPoint) :
    Array DataPoint :=
  match spawnFloorNanos? with
  | none => points
  | some sf =>
    if multiplier ≤ 1.0 || sf == 0 then points
    else
      let threshold : Float := multiplier * sf.toFloat
      points.map fun dp =>
        match dp.status with
        | .ok =>
          if dp.totalNanos.toFloat < threshold then
            { dp with belowSignalFloor := true }
          else dp
        | _ => dp

/-- Measure the per-spawn floor: spawn the child against the first
    available benchmark with a 1µs target. The result is dominated by
    spawn cost when the inner work is trivial.

    The probe forces `cacheMode := .warm` and `targetInnerNanos :=
    1_000` regardless of how the first registered benchmark was
    declared. The point of this number is to characterise the
    harness's own startup cost — it should not vary depending on
    whether the user happened to register a cold-mode benchmark
    first. With warm + tiny target the autotuner converges in a
    single iteration and the wallclock is dominated by spawn /
    process-init / IPC, which is exactly what we want to report. -/
def measureSpawnFloor (env : Env) : IO (Option Nat) := do
  let entries ← allRuntimeEntries
  if entries.isEmpty then return none
  let entry := entries[0]!
  let probeCfg : BenchmarkConfig :=
    { entry.spec.config with
        targetInnerNanos := 1_000
        cacheMode := .warm }
  let t₀ ← IO.monoNanosNow
  -- Pass parent's env along so the probe child takes the same fast
  -- path as a real ladder rung; otherwise the floor measurement is
  -- inflated by the env-capture subprocesses (git / hostname /
  -- /proc/cpuinfo) that real rungs skip.
  let _ ← runOneBatch { entry.spec with config := probeCfg } 0 (some env)
  let t₁ ← IO.monoNanosNow
  return some (t₁ - t₀)

/-- Issue #46: warn on stderr when CLI `--param-floor` /
    `--param-ceiling` are passed but the merged schedule is still
    `.custom`, in which case the override is silently inert. The
    `.custom` arm of `runBenchmark` walks the declared array verbatim;
    floor and ceiling only steer the doubling/linear ladder generators.
    Stay quiet when the user also passed `--param-schedule
    doubling|linear` (which switches the shape away from `.custom`,
    making floor/ceiling actually apply). -/
private def warnInertParamOverrides (name : Lean.Name)
    (override : ConfigOverride) (cfg : BenchmarkConfig) : IO Unit := do
  match cfg.paramSchedule with
  | .custom _ =>
    let mut inert : Array String := #[]
    if override.paramFloor?.isSome   then inert := inert.push "--param-floor"
    if override.paramCeiling?.isSome then inert := inert.push "--param-ceiling"
    unless inert.isEmpty do
      let flags := String.intercalate ", " inert.toList
      IO.eprintln s!"note: {flags} ignored for {name}: effective `paramSchedule` is `.custom`, which walks the declared params verbatim; pass `--param-schedule doubling` (or `linear`) to switch the shape, or edit the `.custom` array."
  | _ => pure ()

/-- Run one benchmark end-to-end: resolve the schedule, run the
    doubling probe, optionally do a bracket-internal linear sweep,
    then summarise.

    The optional `override` lets callers (typically the CLI) override
    individual `BenchmarkConfig` fields on top of whatever was declared
    via `setup_benchmark`. The merged config is validated before any
    subprocesses spawn, so e.g. a negative `--max-seconds-per-call`
    or `paramFloor > paramCeiling` produces a user-facing error rather
    than an empty ladder.

    The optional `deadline?` (issue #9) is an absolute monotonic-clock
    instant past which the ladder stops scheduling new rungs. The
    result's `budgetTruncated` field flips to `true` when the deadline
    cuts the run short. Each child spawn is still independently capped
    by `maxSecondsPerCall`, so the budget can be exceeded by at most
    one in-flight rung. -/
def runBenchmark (name : Lean.Name) (override : ConfigOverride := {})
    (deadline? : Option BudgetDeadline := none) :
    IO BenchmarkResult := do
  let some entry ← findRuntimeEntry name
    | throw (.userError s!"unregistered benchmark: {name}")
  let cfg := override.apply entry.spec.config
  match cfg.validate with
  | .error msg => throw (.userError s!"{name}: {msg}")
  | .ok () => pure ()
  warnInertParamOverrides name override cfg
  -- Capture reproducibility metadata at *parent* run start (issue
  -- #11). The same snapshot is propagated to every child via
  -- `--env-json`, so all JSONL rows in this run stamp the *same*
  -- env (timestamps, git_dirty, hostname all consistent across
  -- rungs). Avoids ~3 subprocess spawns per ladder rung that the
  -- child's fallback fresh-capture would do.
  let env ← RunEnv.capture
  let spec := { entry.spec with config := cfg }
  let schedule := resolveSchedule cfg entry.complexity
  -- `.custom` skips the doubling probe entirely — we walk the
  -- user-supplied params in order and stop when one hits the cap.
  -- For all other schedules the probe runs first and then optionally
  -- a linear bracket sweep is added.
  let mut points : Array DataPoint := #[]
  let mut budgetTruncated := false
  match schedule with
  | .custom params =>
    for n in params do
      if (← deadlineExceeded? deadline?) then
        budgetTruncated := true
        break
      let rung ← runRungTrials spec n cfg.outerTrials (some env)
      points := points ++ rung
      -- Stop the ladder only when EVERY trial at this rung failed.
      -- A transient first-trial timeout no longer poisons the rest
      -- of the schedule (Codex review).
      unless rungAnyOk rung do break
  | _ =>
    let (probePoints, lastOk?, firstFail?, probeBudgetCut) ←
      runDoublingProbe spec env deadline?
    points := probePoints
    if probeBudgetCut then budgetTruncated := true
    -- Skip post-probe work when the probe was budget-truncated;
    -- we already lack a complete bracket and additional spawns
    -- would burn over-budget without improving the verdict.
    if budgetTruncated then pure () else
    match schedule with
    | .doubling =>
      -- Probe rows ARE the verdict data here. Top up every successful
      -- probe rung to `outerTrials` measurements (subject to budget).
      let (topped, cut) ← topUpDoublingTrials spec env points deadline?
      points := topped
      if cut then budgetTruncated := true
    | .custom _ => pure ()  -- unreachable in this branch
    | .auto => pure ()  -- unreachable after resolveSchedule
    | .linear samples =>
      match lastOk?, firstFail? with
      | some lastOk, some firstFail =>
        -- The probe ran one trial per rung; for the bracket-tightening
        -- extrapolation use that single perCallNanos directly. (No
        -- median needed because there's no cluster yet — the cluster
        -- is built later by the sweep.)
        let lastOkPerCall : Float :=
          (probePoints.findSome? fun dp =>
              if dp.param == lastOk && dp.status == .ok then
                some dp.perCallNanos
              else none).getD 0.0
        let capNanos : Float := cfg.maxSecondsPerCall * 1.0e9
        let effFirstFail :=
          estimateFirstFail entry.complexity lastOk lastOkPerCall
            capNanos firstFail
        let rungs := pickLinearRungs lastOk effFirstFail samples
        if !rungs.isEmpty then
          -- Probe rows are bracket-finding only; demote them so they
          -- don't enter `cMin`/`cMax`/slope.
          points := points.map ({ · with partOfVerdict := false })
          for n in rungs do
            if (← deadlineExceeded? deadline?) then
              budgetTruncated := true
              break
            let rung ← runRungTrials spec n cfg.outerTrials (some env)
            points := points ++ rung
            -- Stop the sweep only when EVERY trial at this rung failed.
            unless rungAnyOk rung do break
      | _, _ =>
        -- No bracket (doubling ran clean to ceiling, or first rung failed).
        -- Fall back to doubling-only — top up trials so the verdict
        -- cluster is the full configured size.
        let (topped, cut) ← topUpDoublingTrials spec env points deadline?
        points := topped
        if cut then budgetTruncated := true
  let spawnFloor ← measureSpawnFloor env
  -- Annotate below-floor rows BEFORE summarising so the verdict
  -- ratios and the advisory list see the same filtered view.
  let annotated :=
    annotateBelowSignalFloor cfg.signalFloorMultiplier spawnFloor points
  let summary := Stats.summarize spec entry.complexity annotated
  return { summary with
    spawnFloorNanos? := spawnFloor,
    env? := some env,
    budgetTruncated }

/-! ## Fixed-benchmark orchestration -/

/-- Parse one fixed-benchmark JSONL row into a `FixedDataPoint`.
    Same schema-compatibility contract as `parseChildRow`. -/
def parseFixedChildRow (line : String) : Except String FixedDataPoint := do
  let json ← Json.parse line
  Schema.checkVersion json
  Schema.checkKind Schema.kindFixed json
  let _ ← json.getObjValAs? String "function"
  let repeatIndex ← json.getObjValAs? Nat "repeat_index"
  let totalNanos ← json.getObjValAs? Nat "total_nanos"
  let resultHash : Option UInt64 :=
    match json.getObjVal? "result_hash" with
    | .ok (.str s) => parseHexU64 s
    | _ => none
  let statusStr ← json.getObjValAs? String "status"
  let status : Status := match statusStr with
    | "ok" => .ok
    | "timed_out" => .timedOut
    | "killed_at_cap" => .killedAtCap
    | "error" =>
        .error ((json.getObjValAs? String "error").toOption.getD "")
    | _ => .error s!"unknown status: {statusStr}"
  let allocBytes : Option Nat :=
    match json.getObjValAs? Nat "alloc_bytes" with
    | .ok n => some n
    | .error _ => none
  let peakRssKb : Option Nat :=
    match json.getObjValAs? Nat "peak_rss_kb" with
    | .ok n => some n
    | .error _ => none
  return { repeatIndex, totalNanos, resultHash, status,
           allocBytes, peakRssKb }

private def synthFixedRow (idx : Nat) (status : Status) : FixedDataPoint :=
  { repeatIndex := idx, totalNanos := 0, resultHash := none, status }

/-- Pure classifier for the post-spawn outcome of a fixed-benchmark
    child run. Mirrors `classifyChildOutcome` for the parametric path. -/
def classifyFixedChildOutcome
    (repeatIndex : Nat) (exit : UInt32) (stdout stderrText : String)
    (wasKilled : Bool) : FixedDataPoint :=
  let withStderr (msg : String) : String :=
    if stderrText.isEmpty then msg else s!"{msg}; stderr: {stderrText}"
  if wasKilled then
    synthFixedRow repeatIndex .killedAtCap
  else match exit with
    | 0 =>
      let line := stdout.trimAscii.toString
      if line.isEmpty then
        synthFixedRow repeatIndex
          (.error (withStderr "child exited 0 but produced no output"))
      else match parseFixedChildRow line with
        | .ok row => row
        | .error e =>
          synthFixedRow repeatIndex (.error (withStderr s!"parse: {e}"))
    | other =>
      synthFixedRow repeatIndex
        (.error (withStderr s!"child exited with code {other}"))

/-- Spawn one fixed-benchmark child. `repeatIndex` is the 0-based
    index reported back in the JSONL row; `cfg` carries the kill
    cap. Same kill-on-cap machinery as `runOneBatch`. The optional
    `env?` mirrors `runOneBatch`: when `some`, the parent's snapshot
    rides via `--env-json` and the child skips its own capture; when
    `none`, the child captures fresh (used by `verifyFixedOne`). -/
def runFixedOneBatch (spec : FixedSpec) (cfg : FixedBenchmarkConfig)
    (repeatIndex : Nat) (env? : Option Env := none) : IO FixedDataPoint := do
  let exe ← ownExe
  let deadlineMs : UInt32 :=
    (cfg.maxSecondsPerCall * 1000.0 + cfg.killGraceMs.toFloat).toUInt32
  let baseArgs : Array String := #[
    "_child",
    "--bench", spec.name.toString (escape := false),
    "--fixed",
    "--repeat-index", toString repeatIndex
  ]
  let args : Array String :=
    match env? with
    | some env => baseArgs ++ #["--env-json", (RunEnv.toJson env).compress]
    | none     => baseArgs
  let child ← IO.Process.spawn {
    cmd := exe
    args := args
    stdout := .piped
    stderr := .piped
    stdin := .null
  }
  let stderrTask ← child.stderr.readToEnd.asTask
  let mainDoneRef ← IO.mkRef false
  let killedRef ← IO.mkRef false
  let mutex ← Std.BaseMutex.new
  let _killer ← IO.asTask do
    let pollMs : UInt32 := 25
    let deadline : Nat := deadlineMs.toNat
    let mut waited : Nat := 0
    let mut bailed := false
    while waited < deadline && !bailed do
      if (← mainDoneRef.get) then
        bailed := true
      else
        let step : Nat := min pollMs.toNat (deadline - waited)
        IO.sleep step.toUInt32
        waited := waited + step
    unless bailed do
      mutex.lock
      unless (← mainDoneRef.get) do
        killedRef.set true
        (try child.kill catch _ => pure ())
      mutex.unlock
  let stdout ← child.stdout.readToEnd
  mutex.lock
  mainDoneRef.set true
  mutex.unlock
  let exit ← child.wait
  let stderrText : String :=
    match stderrTask.get with
    | .ok s => s.trimAscii.toString
    | .error _ => ""
  let wasKilled ← killedRef.get
  return classifyFixedChildOutcome repeatIndex exit stdout stderrText wasKilled

/-- Numeric median of a sorted `Array Nat`. Returns the upper of the
    two middle elements for even sizes — i.e. the element at index
    `size / 2` after sorting ascending. No float arithmetic; the raw
    nanosecond values are reported as-is.

    The "upper middle on even" convention is conservative for
    benchmark reporting: it errs toward the slower of two middle
    samples, which is the safer default if you're using the median
    to flag regressions. The min/max columns expose the bracket so
    the choice is visible. -/
private def medianNat (sorted : Array Nat) : Option Nat :=
  if sorted.isEmpty then none
  else some sorted[sorted.size / 2]!

/-- Are all `ok` repeats hash-consistent? Repeats with `none` hashes
    (non-Hashable type) all agree vacuously; mixed `some`/`none`
    shouldn't happen but is treated as agreement (the registration
    macro decides Hashable up front). -/
private def computeHashAgreement (points : Array FixedDataPoint) : Bool := Id.run do
  let okHashes : Array UInt64 := points.filterMap fun dp =>
    match dp.status, dp.resultHash with
    | .ok, some h => some h
    | _,   _      => none
  if okHashes.isEmpty then return true
  let h₀ := okHashes[0]!
  return okHashes.all (· == h₀)

/-- Run one fixed benchmark end-to-end: optional warmup, then
    `cfg.repeats` measured calls. Each measured call is its own
    child spawn so the kill-on-cap machinery covers it.

    The optional `deadline?` (issue #9) lets the caller stop the
    repeat loop early when a CI budget has been exhausted; the
    result's `budgetTruncated` field flips to `true` in that case. -/
def runFixedBenchmark (name : Lean.Name)
    (override : FixedConfigOverride := {})
    (deadline? : Option BudgetDeadline := none) : IO FixedResult := do
  let some entry ← findFixedRuntimeEntry name
    | throw (.userError s!"unregistered fixed benchmark: {name}")
  let cfg := override.apply entry.spec.config
  match cfg.validate with
  | .error msg => throw (.userError s!"{name}: {msg}")
  | .ok () => pure ()
  let env ← RunEnv.capture
  -- Warmup: fire-and-forget; we don't fail the run if warmup itself
  -- fails, but we do propagate killed/error status so the user sees
  -- it (recorded as repeatIndex = 0 in the warmup pass — but not
  -- pushed into `points`).
  if cfg.warmup then
    let _ ← runFixedOneBatch entry.spec cfg 0 (some env)
  let mut points : Array FixedDataPoint := #[]
  let mut budgetTruncated := false
  for i in [0 : cfg.repeats] do
    if (← deadlineExceeded? deadline?) then
      budgetTruncated := true
      break
    let dp ← runFixedOneBatch entry.spec cfg i (some env)
    points := points.push dp
  let okNanos : Array Nat := points.filterMap fun dp =>
    match dp.status with
    | .ok => some dp.totalNanos
    | _   => none
  let sorted := okNanos.qsort (· < ·)
  let medianNanos? := medianNat sorted
  let minNanos? := sorted[0]?
  let maxNanos? := if sorted.isEmpty then none else some sorted[sorted.size - 1]!
  let hashesAgree := computeHashAgreement points
  return {
    function := name
    hashable := entry.spec.hashable
    config := cfg
    points
    medianNanos?
    minNanos?
    maxNanos?
    hashesAgree
    env? := some env
    budgetTruncated
  }

end LeanBench
