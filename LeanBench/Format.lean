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

/-- Format a `Float` with exactly 3 decimal places (trailing zeros
padded, never stripped). `26.0 → "26.000"`, `20.147 → "20.147"`,
`1.498621 → "1.499"`, `0.0005 → "0.001"`. Keeping the decimal width
uniform is what lets `alignDecimals` keep every column's decimal
point on the same vertical line. -/
private def fmtFloat3 (f : Float) : String := Id.run do
  let neg := f < 0.0
  let abs := if neg then -f else f
  let scaled : Nat := (abs * 1000.0).round.toUInt64.toNat
  let whole : Nat := scaled / 1000
  let frac  : Nat := scaled % 1000
  let sign := if neg then "-" else ""
  let fracRaw := toString frac
  let padded := String.ofList (List.replicate (3 - fracRaw.length) '0') ++ fracRaw
  return s!"{sign}{whole}.{padded}"

/-- Pretty-print a duration as `(numeric, unit)` so callers can align
the number and unit independently. -/
def fmtNanos (n : Nat) : String × String :=
  let f : Float := n.toFloat
  if f < 1_000.0 then (fmtFloat3 f, "ns")
  else if f < 1_000_000.0 then (fmtFloat3 (f / 1_000.0), "µs")
  else if f < 1_000_000_000.0 then (fmtFloat3 (f / 1_000_000.0), "ms")
  else (fmtFloat3 (f / 1_000_000_000.0), "s")

/-- Duration as a single space-joined string, for prose lines. -/
def fmtNanosStr (n : Nat) : String :=
  let (num, unit) := fmtNanos n
  s!"{num} {unit}"

/-- Pad on the left with spaces to width `w` (i.e. right-align). -/
private def leftpad (s : String) (w : Nat) : String :=
  let pad := w - s.length
  if pad > 0 then String.ofList (List.replicate pad ' ') ++ s else s

/-- Pad on the right with spaces to width `w` (i.e. left-align). -/
private def rightpad (s : String) (w : Nat) : String :=
  let pad := w - s.length
  if pad > 0 then s ++ String.ofList (List.replicate pad ' ') else s

/-- Given a column of numeric strings (from `fmtFloat3`), pad them so
decimal points line up. Strings without a decimal point get trailing
spaces in place of the missing `.frac`. The em-dash `"—"` passes
through as an integer-only entry and lands in the integer slot. -/
private def alignDecimals (ss : Array String) : Array String := Id.run do
  let split : Array (String × String) := ss.map fun s =>
    match s.splitOn "." with
    | [i]         => (i, "")
    | i :: f :: _ => (i, f)
    | _           => (s, "")
  let maxInt  := split.foldl (fun m (i, _) => max m i.length) 0
  let maxFrac := split.foldl (fun m (_, f) => max m f.length) 0
  return split.map fun (i, f) =>
    let intPad := leftpad i maxInt
    if maxFrac == 0 then
      intPad
    else if f.isEmpty then
      intPad ++ String.ofList (List.replicate (maxFrac + 1) ' ')
    else
      intPad ++ "." ++ rightpad f maxFrac

/-- A single table row, pre-formatted into aligned string parts. -/
private structure Row where
  param       : String
  perCallNum  : String
  perCallUnit : String
  repeats     : String
  cStr        : String
  status      : String
  deriving Inhabited

private def statusSuffix : Status → String
  | .ok          => ""
  | .timedOut    => " [timed out]"
  | .killedAtCap => " [killed at cap]"
  | .error msg   => s!" [error: {msg}]"

/-- Decimal with `_` every three digits from the right: `1123456 → "1_123_456"`. -/
private def fmtNatUnderscores (n : Nat) : String := Id.run do
  let s := toString n
  let len := s.length
  if len ≤ 3 then return s
  let chars := s.toList
  let mut out : String := ""
  for i in [0 : len] do
    if i > 0 ∧ (len - i) % 3 == 0 then
      out := out.push '_'
    out := out.push chars[i]!
  return out

/-- Render the inner-repeat count. The auto-tuner always picks a power
of 2, so we show it as `×2^k`. Non-powers (shouldn't happen in
practice) fall through to a raw decimal, and `0` — what `synthRow`
uses for killed/errored rows — renders as an em-dash. -/
private def fmtRepeats (n : Nat) : String :=
  if n == 0 then "—"
  else
    let k := Nat.log2 n
    if (1 <<< k) == n then s!"×2^{k}"
    else s!"×{n}"

private def rawRow (cMaybe : Option Float) (dp : DataPoint) : Row :=
  let (num, unit) := match dp.status with
    | .ok => fmtNanos dp.perCallNanos.toUInt64.toNat
    | _   => ("—", "")
  let cStr := match cMaybe with
    | some c => fmtFloat3 c
    | none   => "—"
  { param       := fmtNatUnderscores dp.param
    perCallNum  := num
    perCallUnit := unit
    repeats     := fmtRepeats dp.innerRepeats
    cStr
    status      := statusSuffix dp.status }

/-- Render one `BenchmarkResult` as a multi-line block with every
numeric value bounded to 3 decimals and every column width derived
from the data so decimal points align. -/
def fmtResult (r : BenchmarkResult) : String := Id.run do
  let headerLine := s!"{r.function}    expected complexity: {r.complexityFormula}"
  let ratioMap : Std.HashMap Nat Float :=
    r.ratios.foldl (fun m (p, c) => m.insert p c) {}
  let droppedParams : Array Nat :=
    (r.ratios.extract 0 r.verdictDroppedLeading).map (·.1)
  let raws : Array Row := r.points.map fun dp =>
    let row := rawRow (ratioMap.get? dp.param) dp
    if droppedParams.contains dp.param then
      { row with status := row.status ++ " †" }
    else row
  let alignedNums := alignDecimals (raws.map (·.perCallNum))
  let alignedCs   := alignDecimals (raws.map (·.cStr))
  let rows : Array Row := Id.run do
    let mut out : Array Row := #[]
    for i in [0 : raws.size] do
      let r := raws[i]!
      out := out.push { r with
        perCallNum := alignedNums[i]!
        cStr       := alignedCs[i]! }
    return out
  let wParam := rows.foldl (fun m r => max m r.param.length) "param".length
  let wNum   := if alignedNums.size == 0 then 0 else alignedNums[0]!.length
  let wUnit  := rows.foldl (fun m r => max m r.perCallUnit.length) 0
  let wRep   := rows.foldl (fun m r => max m r.repeats.length) "repeats".length
  let wC     := rows.foldl (fun m r => max m r.cStr.length) "C".length
  let perCallColWidth := max "per-call".length (wNum + 1 + wUnit)
  let wUnitEff := perCallColWidth - wNum - 1
  let headerRow :=
    "  " ++ leftpad "param" wParam ++
    "  " ++ rightpad "per-call" perCallColWidth ++
    "  " ++ rightpad "repeats" wRep ++
    "  C"
  let dataLines : Array String := rows.map fun r =>
    let perCall := r.perCallNum ++ " " ++ rightpad r.perCallUnit wUnitEff
    "  " ++ leftpad r.param wParam ++
    "  " ++ perCall ++
    "  " ++ rightpad r.repeats wRep ++
    "  C=" ++ rightpad r.cStr wC ++ r.status
  let mut lines : Array String := #[headerLine, headerRow]
  for line in dataLines do
    lines := lines.push line
  let cMin := match r.cMin? with | some c => fmtFloat3 c | none => "—"
  let cMax := match r.cMax? with | some c => fmtFloat3 c | none => "—"
  let fmtSignedSlope (β : Float) : String :=
    let mag := fmtFloat3 β.abs
    if β < 0.0 then "-" ++ mag else "+" ++ mag
  let slopeStr := match r.slope? with
    | some β => fmtSignedSlope β
    | none   => "—"
  let hint := match r.slope? with
    | some β =>
      let a := β.abs
      if a > r.config.slopeTolerance && a ≥ 0.05 then
        let dir := if β > 0.0 then "slower" else "faster"
        s!", looks {dir} than declared by ~n^{fmtFloat3 a}"
      else ""
    | none => ""
  lines := lines.push
    s!"  verdict: {r.verdict.describe} (cMin={cMin}, cMax={cMax}, β={slopeStr}{hint})"
  match r.spawnFloorNanos? with
  | some n =>
    lines := lines.push s!"  per-spawn floor (harness self-measurement): {fmtNanosStr n}"
  | none => pure ()
  return "\n".intercalate lines.toList

/-- One-line summary of a registered benchmark, used by `list`. -/
def fmtSpec (spec : BenchmarkSpec) : String :=
  let h := if spec.hashable then "" else "  (no Hashable)"
  s!"  {spec.name}    expected complexity: {spec.complexityFormula}{h}"

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
