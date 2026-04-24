import LeanBench.Core
import LeanBench.Env
import LeanBench.Run
import LeanBench.Format

/-!
# `LeanBench.Compare` — cross-implementation comparison

`compare [name₁, name₂, …]` runs each function through its **own**
doubling ladder (per-function param schedules; each stops at its own
`maxSecondsPerCall`), then summarizes:

- the full per-function ladder is preserved
- the **common params** (intersection of every function's measured
  param set) are computed
- if every compared function has a Hashable return type, output
  hashes are checked at every common param; report `allAgreed` /
  `divergedAt` / `hashUnavailable`

This satisfies the user's stated preference (per-function ladders so
each function runs as far as it can) while preserving Codex's
apples-to-apples summary suggestion (common subset for the agreement
check and any future ratio summary stats).
-/

open Lean

namespace LeanBench

/-- Intersection of multiple `Array Nat`s preserving order from the first. -/
private def intersectParams (lists : Array (Array Nat)) : Array Nat := Id.run do
  if lists.isEmpty then return #[]
  let first := lists[0]!
  let rest := lists.extract 1 lists.size
  let mut acc : Array Nat := #[]
  for p in first do
    if rest.all (·.contains p) then
      acc := acc.push p
  return acc

/-- Compute `agreeOnCommon` from the per-function results. -/
private def computeAgreement
    (results : Array BenchmarkResult)
    (commonParams : Array Nat) :
    AgreementStatus := Id.run do
  -- Build (function-name × Std.HashMap Nat (Option UInt64)) per result.
  let perFn : Array (Lean.Name × Std.HashMap Nat (Option UInt64)) := results.map fun r =>
    (r.function, r.points.foldl (fun m dp => m.insert dp.param dp.resultHash) {})
  -- A function whose return type isn't Hashable will have all `none` hashes.
  let anyNoHash := perFn.any fun (_, m) =>
    m.toArray.all (fun (_, h) => h.isNone)
  if anyNoHash then return .hashUnavailable
  let mut diverged : Array (Lean.Name × Lean.Name × Nat) := #[]
  for p in commonParams do
    let hashes : Array (Lean.Name × Option UInt64) := perFn.map fun (n, m) => (n, m.get! p)
    -- Skip params where any function didn't produce a hash (e.g.
    -- killed at cap before emitting). That isn't divergence, just
    -- absence of data.
    if hashes.any (fun (_, h) => h.isNone) then continue
    if hashes.size ≥ 2 then
      let (n₀, h₀) := hashes[0]!
      for i in [1:hashes.size] do
        let (nᵢ, hᵢ) := hashes[i]!
        if hᵢ != h₀ then
          diverged := diverged.push (n₀, nᵢ, p)
  if diverged.isEmpty then .allAgreed else .divergedAt diverged

/-- Run a `compare` over the listed benchmarks. -/
def compare (names : List Lean.Name) : IO ComparisonReport := do
  let mut results : Array BenchmarkResult := #[]
  for n in names do
    results := results.push (← runBenchmark n)
  let allParams := results.map fun r => r.points.map (·.param)
  let common := intersectParams allParams
  let agree := computeAgreement results common
  return { results, commonParams := common, agreeOnCommon := agree }

end LeanBench
