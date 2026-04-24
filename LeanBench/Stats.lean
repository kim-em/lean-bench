import Lean
import LeanBench.Core

/-!
# `LeanBench.Stats` — ratios + verdict

Per the v0.1 spec: the verdict is intentionally weak. The raw
`ratios` array is the source of truth; the verdict is decoration.

This module is pure: it operates on a `BenchmarkSpec`, the collected
`DataPoint`s, and the user's complexity closure, all of which are
passed in by `Run.lean`. No registries are consulted here.
-/

open Lean

namespace LeanBench
namespace Stats

/-- Compute the per-point ratio C = perCallNanos / complexity(param).
    Skips param=0 and param=1 (denominator may be 0 or 1, noisy)
    and any non-`ok` data points. -/
def ratiosFromPoints
    (complexity : Nat → Nat)
    (points : Array DataPoint) :
    Array Ratio := Id.run do
  let mut acc : Array Ratio := #[]
  for dp in points do
    if dp.param < 2 then continue
    if dp.status != .ok then continue
    let d := complexity dp.param
    if d == 0 then continue
    acc := acc.push (dp.param, dp.perCallNanos / d.toFloat)
  return acc

/-- Reduce the ratios into the v0.1 weak verdict. -/
def deriveVerdict (ratios : Array Ratio) : Verdict × Option Float × Option Float :=
  if ratios.size < 3 then
    (.inconclusive, none, none)
  else
    let cs := ratios.map (·.2)
    let cMin := cs.foldl min cs[0]!
    let cMax := cs.foldl max cs[0]!
    let v : Verdict :=
      if cMax / cMin ≤ 4.0 then .consistentWithDeclaredComplexity
      else .inconclusive
    (v, some cMin, some cMax)

/-- Build a `BenchmarkResult` from spec + complexity closure + collected points. -/
def summarize
    (spec : BenchmarkSpec)
    (complexity : Nat → Nat)
    (points : Array DataPoint) :
    BenchmarkResult :=
  let ratios := ratiosFromPoints complexity points
  let (verdict, cMin?, cMax?) := deriveVerdict ratios
  { function := spec.name
  , complexity := spec.complexityName
  , config := spec.config
  , points
  , ratios
  , verdict
  , cMin?
  , cMax? }

end Stats
end LeanBench
