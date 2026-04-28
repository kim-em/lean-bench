import Lean
import LeanBench.Core
import LeanBench.Env
import LeanBench.Stats

/-!
# `LeanBench.Run` — parent-side orchestration

For each param in the doubling ladder:

1. Spawn the same binary as a child (`_child --bench NAME --param N
   --target-nanos T`) with a wallclock cap enforced via the POSIX
   `timeout(1)` utility. We delegate the kill semantics to coreutils
   on purpose — it's simpler and more reliable than rolling our own
   SIGTERM/SIGKILL escalation in Lean. (Linux/macOS only; documented.)
2. Read the child's stdout (one JSONL row).
3. If the child exited 0 with a parseable row → use it.
   If `timeout` returned 124 (graceful timeout) or 137 (SIGKILL after
   `--kill-after`) → synthesize a `killedAtCap` row.
   If the child died otherwise → synthesize an `error` row.
4. Stop doubling once the param hits `paramCeiling` or any single
   batch returns a non-`ok` status.
5. Hand the points to `Stats.summarize` for ratio + verdict.
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

/-- Parse the child's JSONL row into a `DataPoint`. -/
def parseChildRow (line : String) : Except String DataPoint := do
  let json ← Json.parse line
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
  , resultHash := none, status }

/-- Spawn one child invocation and return the resulting DataPoint. -/
def runOneBatch (spec : BenchmarkSpec) (param : Nat) : IO DataPoint := do
  let exe ← ownExe
  let target := spec.config.targetInnerNanos
  let cap : Float := spec.config.maxSecondsPerCall.toFloat
  let grace : Float := spec.config.killGraceMs.toFloat / 1000.0
  let args : Array String := #[
    "--kill-after", s!"{grace}",
    s!"{cap}",
    exe, "_child",
    "--bench", spec.name.toString (escape := false),
    "--param", toString param,
    "--target-nanos", toString target
  ]
  let child ← IO.Process.spawn {
    cmd := "timeout"
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
  let stdout ← child.stdout.readToEnd
  let exit ← child.wait
  let stderrText : String :=
    match stderrTask.get with
    | .ok s => s.trimAscii.toString
    | .error _ => ""
  let withStderr (msg : String) : String :=
    if stderrText.isEmpty then msg else s!"{msg}; stderr: {stderrText}"
  match exit with
  | 0 =>
    let line := stdout.trimAscii.toString
    if line.isEmpty then
      return synthRow param (.error (withStderr "child exited 0 but produced no output"))
    match parseChildRow line with
    | .ok row => return row
    | .error e => return synthRow param (.error (withStderr s!"parse: {e}"))
  | 124 => return synthRow param .killedAtCap
  | 137 => return synthRow param .killedAtCap
  | other => return synthRow param (.error (withStderr s!"child exited with code {other}"))

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

/-- Run one benchmark end-to-end. -/
def runBenchmark (name : Lean.Name) : IO BenchmarkResult := do
  let some entry ← findRuntimeEntry name
    | throw (.userError s!"unregistered benchmark: {name}")
  let ladder := paramLadder entry.spec.config
  let mut points : Array DataPoint := #[]
  let mut continueLadder := true
  for param in ladder do
    if !continueLadder then break
    let dp ← runOneBatch entry.spec param
    points := points.push dp
    match dp.status with
    | .ok => pure ()
    | _ => continueLadder := false
  let spawnFloor ← measureSpawnFloor
  let summary := Stats.summarize entry.spec entry.complexity points
  return { summary with spawnFloorNanos? := spawnFloor }

end LeanBench
