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

/-- Compute `agreeOnCommon` from the per-function results.
First-divergence-first: the earliest entry in `commonParams` where
any pair disagrees becomes `details[0]`.

The `hashUnavailable` test is registration-keyed (off
`BenchmarkResult.hashable`), not data-keyed: a hashable benchmark
whose runs all failed before emitting a hash is NOT misreported as
"no Hashable instance" — that would falsely point the user at the
registration when the real issue is the run.

Exposed (not `private`) so the regression test for the
registration-keyed Hashable check can drive it without needing a
live registry. -/
def computeAgreement
    (results : Array BenchmarkResult)
    (commonParams : Array Nat) :
    AgreementStatus := Id.run do
  let unhashed : Array Lean.Name := results.filterMap fun r =>
    if !r.hashable then some r.function else none
  if !unhashed.isEmpty then return .hashUnavailable unhashed
  -- Build (function-name × Std.HashMap Nat (Option UInt64)) per result.
  let perFn : Array (Lean.Name × Std.HashMap Nat (Option UInt64)) := results.map fun r =>
    (r.function, r.points.foldl (fun m dp => m.insert dp.param dp.resultHash) {})
  let mut details : Array DivergenceDetail := #[]
  for p in commonParams do
    let hashes : Array (Lean.Name × Option UInt64) := perFn.map fun (n, m) => (n, m.get! p)
    -- Skip params where any function didn't produce a hash (e.g.
    -- killed at cap before emitting). That isn't divergence, just
    -- absence of data.
    if hashes.any (fun (_, h) => h.isNone) then continue
    if hashes.size < 2 then continue
    let h₀ := hashes[0]!.2.get!
    let mut dissenters : Array Lean.Name := #[]
    for i in [1:hashes.size] do
      let (nᵢ, hᵢ?) := hashes[i]!
      if hᵢ?.get! != h₀ then
        dissenters := dissenters.push nᵢ
    if !dissenters.isEmpty then
      details := details.push { param := p, hashes, dissenters }
  if details.isEmpty then .allAgreed else .divergedAt details

/-- Run a `compare` over the listed benchmarks. The same `override`
applies to every benchmark in the comparison, so e.g.
`--param-ceiling 1024` shortens every ladder uniformly. -/
def compare (names : List Lean.Name) (override : ConfigOverride := {}) :
    IO ComparisonReport := do
  let mut results : Array BenchmarkResult := #[]
  for n in names do
    results := results.push (← runBenchmark n override)
  let allParams := results.map fun r => r.points.map (·.param)
  let common := intersectParams allParams
  let agree := computeAgreement results common
  return { results, commonParams := common, agreeOnCommon := agree }

/-! ## Fixed-benchmark comparison -/

/-- Hash agreement across N fixed-benchmark results. For each
    function we take the *first* `ok` hash (intra-function
    consistency is recorded separately on `FixedResult.hashesAgree`).
    The `hashUnavailable` test is registration-keyed (off
    `FixedResult.hashable`) — a hashable benchmark whose every
    repeat failed is NOT misreported as "no Hashable". When all
    declared-hashable runs at least produced one hash row each, we
    pairwise compare the first listed function's hash against the
    rest and pack the result into a `FixedDivergenceDetail`. -/
def computeFixedAgreement
    (results : Array FixedResult) : FixedAgreementStatus := Id.run do
  let unhashed : Array Lean.Name := results.filterMap fun r =>
    if !r.hashable then some r.function else none
  if !unhashed.isEmpty then return .hashUnavailable unhashed
  let firstHashes : Array (Lean.Name × Option UInt64) := results.map fun r =>
    let h := r.points.findSome? fun dp =>
      match dp.status, dp.resultHash with
      | .ok, some h => some h
      | _,   _      => none
    (r.function, h)
  if firstHashes.size < 2 then return .allAgreed
  -- A hashable benchmark with no successful repeats has `none` here.
  -- Treat that as "no comparable data" rather than divergence — falling
  -- through to the divergence loop with `none` would crash on `.get!`.
  if firstHashes.any (fun (_, h) => h.isNone) then return .allAgreed
  let h₀ := firstHashes[0]!.2.get!
  let mut dissenters : Array Lean.Name := #[]
  for i in [1:firstHashes.size] do
    let (nᵢ, hᵢ?) := firstHashes[i]!
    if hᵢ?.get! != h₀ then
      dissenters := dissenters.push nᵢ
  if dissenters.isEmpty then .allAgreed
  else .diverged { hashes := firstHashes, dissenters }

/-- Run a `compare` over fixed benchmarks. -/
def compareFixed (names : List Lean.Name)
    (override : FixedConfigOverride := {}) : IO FixedComparisonReport := do
  let mut results : Array FixedResult := #[]
  for n in names do
    results := results.push (← runFixedBenchmark n override)
  let agree := computeFixedAgreement results
  return { results, agreeOnHash := agree }

end LeanBench
