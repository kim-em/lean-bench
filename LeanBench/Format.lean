import LeanBench.Core
import LeanBench.Env

/-!
# `LeanBench.Format` — printers

Stdout-friendly tables for `BenchmarkResult` and `ComparisonReport`.
JSONL emission lives in `Child.lean`; this module is read-only on the
parent side.
-/

namespace LeanBench
namespace Format

/-- Pretty-print a duration. -/
def fmtNanos (n : Nat) : String :=
  let f : Float := n.toFloat
  if f < 1_000.0 then
    s!"{f} ns"
  else if f < 1_000_000.0 then
    s!"{f / 1_000.0} µs"
  else if f < 1_000_000_000.0 then
    s!"{f / 1_000_000.0} ms"
  else
    s!"{f / 1_000_000_000.0} s"

private def truncFloat (f : Float) (digits : Nat) : String := Id.run do
  let scale : Float := (10 : Float) ^ digits.toFloat
  let truncated : Float := (f * scale).floor / scale
  return toString truncated

private def leftpad (s : String) (w : Nat) : String :=
  let pad := w - s.length
  if pad > 0 then String.ofList (List.replicate pad ' ') ++ s else s

/-- Pretty-print one `DataPoint` as a single row. -/
def fmtPoint (cMaybe : Option Float) (dp : DataPoint) : String :=
  let param := s!"{dp.param}"
  let perCall := fmtNanos dp.perCallNanos.toUInt64.toNat
  let inner := s!"×{dp.innerRepeats}"
  let cStr := match cMaybe with
    | some c => truncFloat c 3
    | none => "—"
  let status := match dp.status with
    | .ok => ""
    | .timedOut => " [timed out]"
    | .killedAtCap => " [killed at cap]"
    | .error msg => s!" [error: {msg}]"
  s!"  {leftpad param 6} {leftpad perCall 12} {leftpad inner 8}  C={cStr}{status}"

/-- Render one `BenchmarkResult` as a multi-line block. -/
def fmtResult (r : BenchmarkResult) : String := Id.run do
  let header := s!"{r.function}   complexity={r.complexity}"
  -- align ratios to points by param
  let ratioMap : Std.HashMap Nat Float :=
    r.ratios.foldl (fun m (p, c) => m.insert p c) {}
  let mut lines : Array String := #[header, "  param   per-call    repeats  C"]
  for dp in r.points do
    lines := lines.push (fmtPoint (ratioMap.get? dp.param) dp)
  let cMin := match r.cMin? with | some c => truncFloat c 3 | none => "—"
  let cMax := match r.cMax? with | some c => truncFloat c 3 | none => "—"
  lines := lines.push s!"  verdict: {r.verdict.toString} (cMin={cMin}, cMax={cMax})"
  match r.spawnFloorNanos? with
  | some n => lines := lines.push s!"  per-spawn floor (harness self-measurement): {fmtNanos n}"
  | none => ()
  return "\n".intercalate lines.toList

/-- One-line summary of a registered benchmark, used by `list`. -/
def fmtSpec (spec : BenchmarkSpec) : String :=
  let h := if spec.hashable then "" else " (no Hashable)"
  s!"  {spec.name}  →  complexity = {spec.complexityName}{h}"

/-- Render a `ComparisonReport` as a per-function block list plus a
    summary noting the common-param intersection. -/
def fmtComparison (rep : ComparisonReport) : String := Id.run do
  let mut lines : Array String := #[]
  for r in rep.results do
    lines := lines.push (fmtResult r)
    lines := lines.push ""
  let common := rep.commonParams.toList.map toString
  lines := lines.push s!"common params (apples-to-apples): {String.intercalate ", " common}"
  let agreeStr := match rep.agreeOnCommon with
    | .allAgreed => "all functions agree on common params"
    | .hashUnavailable => "no Hashable instance: cannot compare outputs"
    | .divergedAt entries => s!"DIVERGED on {entries.size} (function, function, param) triples"
  lines := lines.push s!"agreement: {agreeStr}"
  return "\n".intercalate lines.toList

end Format
end LeanBench
