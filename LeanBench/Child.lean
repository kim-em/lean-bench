import Lean
import LeanBench.Core
import LeanBench.Env
import LeanBench.Schema

/-!
# `LeanBench.Child` — child-mode runner

The child is invoked by the parent as:

    ./bench _child --bench NAME --param N --target-nanos T

It looks `NAME` up in the runtime registry, runs the function with
auto-tuned inner repeats until total wall ≥ `T / 2`, prints one JSONL
row to stdout, and exits 0. If anything goes wrong it prints an error
JSONL row and exits non-zero.

**Critical:** the timing AND the auto-tuned-repeat loop both happen
on this side of the subprocess. The parent never measures wall time
itself.
-/

open Lean

namespace LeanBench

/-- Smallest power of two `≥ n`. `0 ↦ 1`, `1 ↦ 1`, `5 ↦ 8`. -/
private partial def nextPowerOfTwo : Nat → Nat
  | 0 => 1
  | n =>
    let rec go (p : Nat) : Nat := if p ≥ n then p else go (p * 2)
    go 1

/-- Auto-tuner: probe, then jump. Pick an inner-repeat count whose
batch wall time lands just past `targetNanos / 2`. The count is
always a power of two — the probe doubles from 1, and the jump
rounds its scale factor up to the next power of two — so the
displayed `×2^k` in the report table is exact.

**Phase 1 (probe).** Start `count := 1`, double until one batch
exceeds a tiny "signal floor" (`halfTarget / 256` — ~1 ms for the
default 250 ms half-target). That gives a stable per-call estimate
while burning ≤ ~2 ms of ramp-up. If a probe batch already crosses
`halfTarget`, return it — the function under test is slow enough
that one call does the job.

**Phase 2 (jump).** Extrapolate the measured per-call time, pick a
scale factor (rounded up to the next power of two) that should
overshoot `halfTarget` by ~10%, and run one final batch at
`count * scale`.

Cumulative wall time is therefore `probe (≤ ~2 ms) + final batch
(≤ ~2 × halfTarget after the power-of-two round-up)` — well under
the parent's 1 s `maxSecondsPerCall` cap for the default 500 ms
target.

Returns `(innerRepeats, totalNanos, lastResultHash)`. The hash is
whatever the loop reports — present iff the function's return type
has a `Hashable` instance. The loop closure does its own timing
internally; per-param prep ran in the caller (via the runtime
entry's `runner`), once per spawn, before this autotune even starts. -/
partial def autoTune
    (loop : Nat → IO (Nat × Option UInt64))
    (targetNanos : Nat) :
    IO (Nat × Nat × Option UInt64) := do
  let halfTarget := targetNanos / 2
  let probeFloor := max 1 (halfTarget / 256)
  let runN (n : Nat) : IO (Nat × Option UInt64) := loop n
  let mut count := 1
  let (t₀, h₀) ← runN count
  let mut t := t₀
  let mut h := h₀
  while t < probeFloor && t < halfTarget do
    count := count * 2
    let (t', h') ← runN count
    t := t'
    h := h'
  if halfTarget ≤ t then
    return (count, t, h)
  let scaleRaw : Nat := (halfTarget * 11 + 10 * t - 1) / (10 * t)
  let scale    : Nat := max 2 (nextPowerOfTwo scaleRaw)
  let targetCount := count * scale
  let (tFinal, hFinal) ← runN targetCount
  return (targetCount, tFinal, hFinal)

/-- JSON-escape a string. Minimal — handles the few characters that
matter for the fields we emit. -/
private def jsonEscape (s : String) : String :=
  s.foldl (init := "") fun acc c =>
    match c with
    | '"' => acc ++ "\\\""
    | '\\' => acc ++ "\\\\"
    | '\n' => acc ++ "\\n"
    | '\r' => acc ++ "\\r"
    | '\t' => acc ++ "\\t"
    | c => acc.push c

/-- Render a string field. -/
private def jsonStr (s : String) : String := s!"\"{jsonEscape s}\""

/-- Render an `Option String` field (`null` when none). -/
private def jsonOptStr : Option String → String
  | none => "null"
  | some s => jsonStr s

/-- Render an `Option UInt64` field as a hex literal string or `null`. -/
private def jsonOptHash : Option UInt64 → String
  | none => "null"
  | some h => s!"\"0x{String.ofList (Nat.toDigits 16 h.toNat)}\""

/-- Render one JSONL row to stdout. -/
def emitRow
    (function : Lean.Name) (param innerRepeats totalNanos : Nat)
    (resultHash : Option UInt64) (status : Status)
    (errorMsg? : Option String := none) :
    IO Unit := do
  let perCall : Float := totalNanos.toFloat / innerRepeats.toFloat
  let row :=
    "{" ++ String.intercalate "," [
      s!"\"schema_version\":{Schema.schemaVersion}",
      s!"\"kind\":{jsonStr Schema.kindParametric}",
      s!"\"function\":{jsonStr function.toString}",
      s!"\"param\":{param}",
      s!"\"inner_repeats\":{innerRepeats}",
      s!"\"total_nanos\":{totalNanos}",
      s!"\"per_call_nanos\":{perCall}",
      s!"\"result_hash\":{jsonOptHash resultHash}",
      s!"\"status\":{jsonStr status.toJsonString}",
      s!"\"error\":{jsonOptStr errorMsg?}"
    ] ++ "}"
  IO.println row

/-- Top-level child entry point.

    Catches exceptions thrown by the runner (e.g. a panicking
    function-under-test) and emits a structured `error` row so the
    parent — and `verify` in particular — can show the failure
    reason rather than just an exit code. -/
def runChildMode (benchName : Lean.Name) (param targetNanos : Nat) : IO UInt32 := do
  match ← findRuntimeEntry benchName with
  | none =>
    emitRow benchName param 0 0 none
      (.error s!"unregistered benchmark: {benchName}")
      (some s!"unregistered benchmark: {benchName}")
    return 1
  | some entry =>
    try
      let loop ← entry.runner param
      let (count, total, hash) ← autoTune loop targetNanos
      emitRow benchName param count total hash .ok
      return (0 : UInt32)
    catch e =>
      let msg := s!"runner threw: {e.toString}"
      emitRow benchName param 0 0 none (.error msg) (some msg)
      return (1 : UInt32)

/-! ## Fixed-benchmark child mode

A fixed-benchmark child runs the registered value once, measures
wall time, hashes the result, and emits one JSONL row tagged with
`kind:"fixed"`. The parent invokes one child per measured repeat
(plus one warmup), so the existing kill-on-cap / per-spawn
isolation machinery applies unchanged. -/

/-- Render one fixed-benchmark JSONL row. The schema overlaps the
    parametric row but adds `kind:"fixed"` and a `repeat_index`
    field; `param` / `inner_repeats` are absent because they're
    meaningless for a fixed benchmark. -/
def emitFixedRow
    (function : Lean.Name) (repeatIndex totalNanos : Nat)
    (resultHash : Option UInt64) (status : Status)
    (errorMsg? : Option String := none) :
    IO Unit := do
  let row :=
    "{" ++ String.intercalate "," [
      s!"\"schema_version\":{Schema.schemaVersion}",
      s!"\"kind\":{jsonStr Schema.kindFixed}",
      s!"\"function\":{jsonStr function.toString}",
      s!"\"repeat_index\":{repeatIndex}",
      s!"\"total_nanos\":{totalNanos}",
      s!"\"result_hash\":{jsonOptHash resultHash}",
      s!"\"status\":{jsonStr status.toJsonString}",
      s!"\"error\":{jsonOptStr errorMsg?}"
    ] ++ "}"
  IO.println row

/-- Top-level entry point for fixed-benchmark child runs. Looks the
    name up in the fixed runtime registry, performs one timed
    invocation, emits one JSONL row, exits 0 on success or 1 on
    error. -/
def runFixedChildMode (benchName : Lean.Name) (repeatIndex : Nat) : IO UInt32 := do
  match ← findFixedRuntimeEntry benchName with
  | none =>
    emitFixedRow benchName repeatIndex 0 none
      (.error s!"unregistered fixed benchmark: {benchName}")
      (some s!"unregistered fixed benchmark: {benchName}")
    return 1
  | some entry =>
    try
      let (total, hash) ← entry.runner
      emitFixedRow benchName repeatIndex total hash .ok
      return (0 : UInt32)
    catch e =>
      let msg := s!"runner threw: {e.toString}"
      emitFixedRow benchName repeatIndex 0 none (.error msg) (some msg)
      return (1 : UInt32)

end LeanBench
