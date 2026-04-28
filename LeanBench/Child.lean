import Lean
import LeanBench.Core
import LeanBench.Env

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
whatever the runner reports — present iff the function's return type
has a `Hashable` instance. -/
partial def autoTune
    (runner : Nat → Nat → IO (Option UInt64))
    (param : Nat)
    (targetNanos : Nat) :
    IO (Nat × Nat × Option UInt64) := do
  let halfTarget := targetNanos / 2
  let probeFloor := max 1 (halfTarget / 256)
  let rec runN (n : Nat) : IO (Nat × Option UInt64) := do
    let t₀ ← IO.monoNanosNow
    let hash ← runner n param
    let t₁ ← IO.monoNanosNow
    return (t₁ - t₀, hash)
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
  -- Scale factor is `⌈halfTarget × 1.1 / t⌉`, rounded up to the next
  -- power of two (so `count * scale` stays a power of two), and ≥ 2
  -- so we never re-run the same size twice.
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
      "\"schema_version\":1",
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

/-- Top-level child entry point. -/
def runChildMode (benchName : Lean.Name) (param targetNanos : Nat) : IO UInt32 := do
  match ← findRuntimeEntry benchName with
  | none =>
    emitRow benchName param 0 0 none
      (.error s!"unregistered benchmark: {benchName}")
      (some s!"unregistered benchmark: {benchName}")
    return 1
  | some entry =>
    let (count, total, hash) ← autoTune entry.runner param targetNanos
    emitRow benchName param count total hash .ok
    return 0

end LeanBench
