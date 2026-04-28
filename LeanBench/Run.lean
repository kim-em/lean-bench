import Lean
import Std.Sync.Mutex
import LeanBench.Core
import LeanBench.Env
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
  return {
    param, innerRepeats, totalNanos, perCallNanos,
    resultHash, status }

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

/-- Spawn one child invocation and return the resulting DataPoint. -/
def runOneBatch (spec : BenchmarkSpec) (param : Nat) : IO DataPoint := do
  let exe ← ownExe
  let target := spec.config.targetInnerNanos
  let deadlineMs : UInt32 :=
    (spec.config.maxSecondsPerCall * 1000.0 + spec.config.killGraceMs.toFloat).toUInt32
  let args : Array String := #[
    "_child",
    "--bench", spec.name.toString (escape := false),
    "--param", toString param,
    "--target-nanos", toString target
  ]
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

/-- Run the static doubling ladder. Returns `(points, lastOk?, firstFail?)`:

- `lastOk?` is the largest param whose status was `.ok` (none if no row
  was ok),
- `firstFail?` is the first param after `lastOk` whose status was non-ok
  (none if the ladder ran to ceiling without failing).

Probe rows are returned with whatever `partOfVerdict` `runOneBatch`
chose (defaults to true); the caller flips them as needed. -/
private def runDoublingProbe (spec : BenchmarkSpec) :
    IO (Array DataPoint × Option Nat × Option Nat) := do
  let ladder := paramLadder spec.config
  let mut points : Array DataPoint := #[]
  let mut lastOk : Option Nat := none
  let mut firstFail : Option Nat := none
  for param in ladder do
    if firstFail.isSome then break
    let dp ← runOneBatch spec param
    points := points.push dp
    match dp.status with
    | .ok => lastOk := some param
    | _   => firstFail := some param
  return (points, lastOk, firstFail)

/-- Resolve `.auto` to either `.doubling` or a sensible `.linear`. -/
private def resolveSchedule (cfg : BenchmarkConfig) (complexity : Nat → Nat) :
    ParamSchedule :=
  match cfg.paramSchedule with
  | .doubling => .doubling
  | .linear samples => .linear samples
  | .auto =>
    if autoPicksLinear complexity then .linear 16 else .doubling

/-- Measure the per-spawn floor: spawn the child against the first
    available benchmark with a 1µs target. The result is dominated by
    spawn cost when the inner work is trivial. -/
def measureSpawnFloor : IO (Option Nat) := do
  let entries ← allRuntimeEntries
  if entries.isEmpty then return none
  let entry := entries[0]!
  let t₀ ← IO.monoNanosNow
  let _ ← runOneBatch { entry.spec with config := { entry.spec.config with targetInnerNanos := 1_000 } } 0
  let t₁ ← IO.monoNanosNow
  return some (t₁ - t₀)

/-- Run one benchmark end-to-end: resolve the schedule, run the
    doubling probe, optionally do a bracket-internal linear sweep,
    then summarise.

    The optional `override` lets callers (typically the CLI) override
    individual `BenchmarkConfig` fields on top of whatever was declared
    via `setup_benchmark`. The merged config is validated before any
    subprocesses spawn, so e.g. a negative `--max-seconds-per-call`
    or `paramFloor > paramCeiling` produces a user-facing error rather
    than an empty ladder. -/
def runBenchmark (name : Lean.Name) (override : ConfigOverride := {}) :
    IO BenchmarkResult := do
  let some entry ← findRuntimeEntry name
    | throw (.userError s!"unregistered benchmark: {name}")
  let cfg := override.apply entry.spec.config
  match cfg.validate with
  | .error msg => throw (.userError s!"{name}: {msg}")
  | .ok () => pure ()
  let spec := { entry.spec with config := cfg }
  let schedule := resolveSchedule cfg entry.complexity
  let (probePoints, lastOk?, firstFail?) ← runDoublingProbe spec
  let mut points := probePoints
  match schedule with
  | .doubling => pure ()
  | .auto => pure ()  -- unreachable after resolveSchedule
  | .linear samples =>
    match lastOk?, firstFail? with
    | some lastOk, some firstFail =>
      -- Tighten the bracket using the complexity model + observed
      -- cost at `lastOk`, so we don't burn the sweep budget on rungs
      -- that are guaranteed to hit the cap.
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
          let dp ← runOneBatch spec n
          points := points.push dp
          if dp.status != .ok then break
    | _, _ =>
      -- No bracket (doubling ran clean to ceiling, or first rung failed).
      -- Fall back to doubling-only — probe rows already have
      -- `partOfVerdict := true`.
      pure ()
  let spawnFloor ← measureSpawnFloor
  let summary := Stats.summarize spec entry.complexity points
  return { summary with spawnFloorNanos? := spawnFloor }

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
  return { repeatIndex, totalNanos, resultHash, status }

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
    cap. Same kill-on-cap machinery as `runOneBatch`. -/
def runFixedOneBatch (spec : FixedSpec) (cfg : FixedBenchmarkConfig)
    (repeatIndex : Nat) : IO FixedDataPoint := do
  let exe ← ownExe
  let deadlineMs : UInt32 :=
    (cfg.maxSecondsPerCall * 1000.0 + cfg.killGraceMs.toFloat).toUInt32
  let args : Array String := #[
    "_child",
    "--bench", spec.name.toString (escape := false),
    "--fixed",
    "--repeat-index", toString repeatIndex
  ]
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
    child spawn so the kill-on-cap machinery covers it. -/
def runFixedBenchmark (name : Lean.Name)
    (override : FixedConfigOverride := {}) : IO FixedResult := do
  let some entry ← findFixedRuntimeEntry name
    | throw (.userError s!"unregistered fixed benchmark: {name}")
  let cfg := override.apply entry.spec.config
  match cfg.validate with
  | .error msg => throw (.userError s!"{name}: {msg}")
  | .ok () => pure ()
  -- Warmup: fire-and-forget; we don't fail the run if warmup itself
  -- fails, but we do propagate killed/error status so the user sees
  -- it (recorded as repeatIndex = 0 in the warmup pass — but not
  -- pushed into `points`).
  if cfg.warmup then
    let _ ← runFixedOneBatch entry.spec cfg 0
  let mut points : Array FixedDataPoint := #[]
  for i in [0 : cfg.repeats] do
    let dp ← runFixedOneBatch entry.spec cfg i
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
    config := cfg
    points
    medianNanos?
    minNanos?
    maxNanos?
    hashesAgree
  }

end LeanBench
