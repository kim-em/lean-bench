import LeanBench.Core
import LeanBench.Env
import LeanBench.Verify

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
  let baseStatus := statusSuffix dp.status
  -- Doubling probe rows in `.linear` mode are demoted from the
  -- verdict; mark them so a row with valid timing but `C=—` doesn't
  -- look like a measurement failure.
  let probeMarked :=
    if dp.status == .ok && !dp.partOfVerdict then baseStatus ++ " [probe]"
    else baseStatus
  -- Below-signal-floor rows are excluded from the verdict because
  -- their per-call time is dominated by subprocess overhead — flag
  -- them so users see why `C=—` for an `ok` row. Issue #15.
  let status :=
    if dp.belowSignalFloor then probeMarked ++ " [<floor]"
    else probeMarked
  { param       := fmtNatUnderscores dp.param
    perCallNum  := num
    perCallUnit := unit
    repeats     := fmtRepeats dp.innerRepeats
    cStr
    status }

/-- Suggestion suffix used in several advisories. Pinned here so the
    `where { ... }` literal stays out of `s!"..."` interpolation
    (Lean's interpolation grammar reserves `{` / `}`). -/
private def customScheduleHint : String :=
  "use `where { paramSchedule := .custom #[...] }` to pin a tractable rung set"

/-- Render one `Advisory` for a result as a single advisory line.
Each message is structured as `‼ <symptom>; <suggestion>` so users
see both why the harness is concerned and what knob to turn next.
Issue #15. -/
private def fmtAdvisory (r : BenchmarkResult) : Advisory → String
  | .belowSignalFloor =>
    let floorStr := match r.spawnFloorNanos? with
      | some n => s!" (per-spawn floor ≈ {fmtNanosStr n})"
      | none => ""
    s!"  ‼ every measurement was below {fmtFloat3 r.config.signalFloorMultiplier}× the per-spawn floor{floorStr}; the function is too fast for child-process measurement at the configured params. Try `--param-ceiling` higher, wrap the function in a hot inner loop, or use `setup_fixed_benchmark` for a single-shot reading."
  | .partiallyBelowSignalFloor n total =>
    s!"  ‼ {n}/{total} ok rows were below {fmtFloat3 r.config.signalFloorMultiplier}× the per-spawn floor; those rungs are excluded from the verdict. Raise `--param-floor` to skip the cold regime if the trimmed warmup isn't enough."
  | .allCapped =>
    s!"  ‼ every measurement hit the wallclock cap (`maxSecondsPerCall = {fmtFloat3 r.config.maxSecondsPerCall}s`); no `ok` row landed. The benchmark is too slow at the configured params. Bump `--max-seconds-per-call`, lower `--param-ceiling`, or " ++ customScheduleHint ++ "."
  | .truncatedAtCap p =>
    -- Phrased order-agnostically: "later rungs were skipped" works
    -- for the doubling/linear ladders (ascending) and for `.custom`
    -- (whatever order the user listed). Codex flagged that
    -- "no rungs above this point" was misleading for sparse / non-
    -- monotone custom ladders.
    s!"  ‼ ladder stopped at param={p} after hitting the wallclock cap (`maxSecondsPerCall = {fmtFloat3 r.config.maxSecondsPerCall}s`); subsequent rungs in the schedule were skipped. Raise the cap if you need to reach further, or " ++ customScheduleHint ++ "."
  | .tooFewVerdictRows kept =>
    s!"  ‼ only {kept} verdict-eligible row(s) survived warmup-trim + signal-floor filter; the verdict is below resolution. Lower `--warmup-fraction`, raise `--param-ceiling`, or " ++ customScheduleHint ++ "."

/-- Render one `BenchmarkResult` as a multi-line block with every
numeric value bounded to 3 decimals and every column width derived
from the data so decimal points align. -/
def fmtResult (r : BenchmarkResult) : String := Id.run do
  let modeTag : String :=
    match r.config.cacheMode with
    | .warm => "warm"
    | .cold => "cold"
  let headerLine := s!"{r.function}    expected complexity: {r.complexityFormula}    [{modeTag} cache]"
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
  for adv in r.advisories do
    lines := lines.push (fmtAdvisory r adv)
  return "\n".intercalate lines.toList

/-- One-line summary of a registered benchmark, used by `list`. -/
def fmtSpec (spec : BenchmarkSpec) : String :=
  let h := if spec.hashable then "" else "  (no Hashable)"
  s!"  {spec.name}    expected complexity: {spec.complexityFormula}{h}"

/-- One-line summary of a registered fixed benchmark. The `[fixed]`
    annotation distinguishes it from parametric entries in the list. -/
def fmtFixedSpec (spec : FixedSpec) : String :=
  let h := if spec.hashable then "" else "  (no Hashable)"
  s!"  {spec.name}    [fixed] repeats={spec.config.repeats}{h}"

/-- Render a single fixed-benchmark result as a multi-line block:
    one row per measured repeat plus a summary with median/min/max
    and the cross-repeat hash agreement check. -/
def fmtFixedResult (r : FixedResult) : String := Id.run do
  let header := s!"{r.function}    [fixed] repeats={r.config.repeats}"
  let mut lines : Array String := #[header]
  let mut idx : Nat := 0
  let nums := r.points.map fun dp =>
    match dp.status with
    | .ok =>
      let (n, _) := fmtNanos dp.totalNanos
      n
    | _ => "—"
  let aligned := alignDecimals nums
  let units := r.points.map fun dp =>
    match dp.status with
    | .ok => (fmtNanos dp.totalNanos).2
    | _ => ""
  let wUnit := units.foldl (fun m u => max m u.length) 0
  for dp in r.points do
    let suffix := match dp.status with
      | .ok => ""
      | .timedOut => " [timed out]"
      | .killedAtCap => " [killed at cap]"
      | .error msg => s!" [error: {msg}]"
    let num := aligned[idx]!
    let unit := units[idx]!
    let perCall := num ++ " " ++ rightpad unit wUnit
    lines := lines.push s!"  repeat {idx}  {perCall}{suffix}"
    idx := idx + 1
  let summary := match r.medianNanos?, r.minNanos?, r.maxNanos? with
    | some med, some lo, some hi =>
      s!"  median: {fmtNanosStr med}    min: {fmtNanosStr lo}    max: {fmtNanosStr hi}"
    | _, _, _ => "  median: — (no successful repeats)"
  lines := lines.push summary
  let hashLine := if r.hashesAgree then
      "  hash: all repeats agree"
    else
      "  hash: DIVERGED across repeats — likely a non-deterministic benchmark"
  lines := lines.push hashLine
  return "\n".intercalate lines.toList

/-- `0xDEADBEEF` rendering for an `Option UInt64`; `—` when absent
    (e.g. a function whose return type lacks `Hashable`, or a row
    whose child died before emitting a hash). -/
private def fmtOptHashHex : Option UInt64 → String
  | none   => "—"
  | some h => s!"0x{String.ofList (Nat.toDigits 16 h.toNat)}"

/-- One indented line per compared function: `<name> hash=<hex>`,
    annotated with how it relates to the baseline (the first entry
    in `hashes`). Used by both parametric and fixed comparison
    reporters so the layout stays consistent.

    `dissenters` is the names whose hash differed from the
    baseline's; everyone else is silently agreeing or has no hash to
    compare. -/
private def fmtHashTable
    (hashes     : Array (Lean.Name × Option UInt64))
    (dissenters : Array Lean.Name) : Array String := Id.run do
  if hashes.isEmpty then return #[]
  let baselineName : Lean.Name := hashes[0]!.1
  let dissenterSet : Std.HashSet Lean.Name :=
    dissenters.foldl (init := {}) (fun s n => s.insert n)
  let nameStrs : Array String := hashes.map fun (n, _) => n.toString
  let nameWidth :=
    nameStrs.foldl (fun m s => max m s.length) 0
  let mut lines : Array String := #[]
  let mut i : Nat := 0
  for (n, h) in hashes do
    let suffix : String :=
      if n == baselineName && !dissenters.isEmpty then
        "    (baseline)"
      else if dissenterSet.contains n then
        s!"    differs from {baselineName}"
      else ""
    lines := lines.push <|
      "    " ++ rightpad nameStrs[i]! nameWidth ++
      "  hash=" ++ fmtOptHashHex h ++ suffix
    i := i + 1
  return lines

/-- Render a fixed-benchmark comparison report: one block per
    function plus a relative-timing line and the hash-agreement
    summary. -/
def fmtFixedComparison (rep : FixedComparisonReport) : String := Id.run do
  let mut lines : Array String := #[]
  for r in rep.results do
    lines := lines.push (fmtFixedResult r)
    lines := lines.push ""
  -- Relative timing: pick the first result's median as the baseline,
  -- emit one ratio line per other result.
  match rep.results[0]? with
  | some baseline =>
    match baseline.medianNanos? with
    | some baseNanos =>
      let baseFloat := baseNanos.toFloat
      let mut relLines : Array String :=
        #[s!"relative median (baseline = {baseline.function}, the first CLI argument):"]
      for r in rep.results do
        let label := s!"  {r.function}"
        match r.medianNanos? with
        | some n =>
          let ratio := n.toFloat / baseFloat
          relLines := relLines.push s!"{label}: {fmtFloat3 ratio}× ({fmtNanosStr n})"
        | none =>
          relLines := relLines.push s!"{label}: — (no successful repeats)"
      lines := lines ++ relLines
    | none =>
      lines := lines.push s!"relative median: baseline {baseline.function} has no successful repeats"
  | none => pure ()
  match rep.agreeOnHash with
  | .allAgreed =>
    lines := lines.push "agreement: all functions agree on output"
  | .hashUnavailable unhashed =>
    let names := String.intercalate ", " (unhashed.toList.map (·.toString))
    lines := lines.push s!"agreement: cannot check — no Hashable instance for: {names}"
    lines := lines.push "  (register a Hashable instance on the return type to enable comparison)"
  | .diverged detail =>
    lines := lines.push s!"agreement: DIVERGED — implementations disagree on output:"
    for line in fmtHashTable detail.hashes detail.dissenters do
      lines := lines.push line
    lines := lines.push
      "  (only result hashes are available; see doc/quickstart.md for what to expect)"
  return "\n".intercalate lines.toList

/-- Render one verify report. Passing reports render as a single
    line. Failing reports render as a header line followed by one
    indented line per failed check, so users see every distinct
    failure (e.g. `f 0` and `f 1` may fail for different reasons). -/
def fmtVerifyReport (r : VerifyReport) : String :=
  if r.passed then
    s!"  [ok ] {r.spec.name}"
  else
    let header := s!"  [FAIL] {r.spec.name}"
    let failures := r.checks.filterMap (·.failure)
    let body := failures.toList.map (fun msg => s!"         —  {msg}")
    "\n".intercalate (header :: body)

/-- Render the full verify summary for a list of reports. -/
def fmtVerify (reports : Array VerifyReport) : String := Id.run do
  if reports.isEmpty then
    return "(no benchmarks registered)"
  let mut lines : Array String := #[s!"verifying {reports.size} benchmark(s)..."]
  for r in reports do
    lines := lines.push (fmtVerifyReport r)
  let failed := reports.filter (! ·.passed) |>.size
  let summary :=
    if failed == 0 then s!"all {reports.size} benchmark(s) passed"
    else s!"{failed} of {reports.size} benchmark(s) failed verification"
  lines := lines.push summary
  return "\n".intercalate lines.toList

/-- Render one fixed-benchmark verify report. Same shape as
    `fmtVerifyReport`. -/
def fmtFixedVerifyReport (r : FixedVerifyReport) : String :=
  if r.passed then
    s!"  [ok ] {r.spec.name}  [fixed]"
  else
    let header := s!"  [FAIL] {r.spec.name}  [fixed]"
    let failures := r.checks.filterMap (·.failure)
    let body := failures.toList.map (fun msg => s!"         —  {msg}")
    "\n".intercalate (header :: body)

/-- Render a unified verify summary that covers both registries. -/
def fmtCombinedVerify (r : CombinedVerifyReports) : String := Id.run do
  let total := r.totalCount
  if total == 0 then
    return "(no benchmarks registered)"
  let mut lines : Array String := #[s!"verifying {total} benchmark(s)..."]
  for rep in r.parametric do
    lines := lines.push (fmtVerifyReport rep)
  for rep in r.fixed do
    lines := lines.push (fmtFixedVerifyReport rep)
  let failed := r.failedCount
  let summary :=
    if failed == 0 then s!"all {total} benchmark(s) passed"
    else s!"{failed} of {total} benchmark(s) failed verification"
  lines := lines.push summary
  return "\n".intercalate lines.toList

/-- Render a `ComparisonReport` as a per-function block list plus a
    summary noting the common-param intersection.

    On divergence, the summary is structured to be debuggable: the
    earliest common param with disagreement is named, every compared
    function's hash at that param is laid out side-by-side, and
    later-diverging params are listed compactly. The hash-only
    output is what users see today; full result previews are not
    yet recorded — see [`doc/quickstart.md`](../../doc/quickstart.md)
    for what to expect when only hashes are available. -/
def fmtComparison (rep : ComparisonReport) : String := Id.run do
  let mut lines : Array String := #[]
  for r in rep.results do
    lines := lines.push (fmtResult r)
    lines := lines.push ""
  let common := rep.commonParams.toList.map toString
  lines := lines.push s!"common params (apples-to-apples): {String.intercalate ", " common}"
  match rep.agreeOnCommon with
  | .allAgreed =>
    lines := lines.push "agreement: all functions agree on common params"
  | .hashUnavailable unhashed =>
    let names := String.intercalate ", " (unhashed.toList.map (·.toString))
    lines := lines.push s!"agreement: cannot check — no Hashable instance for: {names}"
    lines := lines.push "  (register a Hashable instance on the return type to enable comparison)"
  | .divergedAt details =>
    let n := details.size
    let plural := if n == 1 then "param" else "params"
    let first := details[0]!
    lines := lines.push <|
      s!"agreement: DIVERGED on {n} {plural} — earliest divergence at param={first.param}:"
    for line in fmtHashTable first.hashes first.dissenters do
      lines := lines.push line
    if details.size > 1 then
      let later := details.extract 1 details.size
      let laterParams := String.intercalate ", "
        (later.toList.map (fun d => toString d.param))
      lines := lines.push s!"  also diverged at: {laterParams}"
    lines := lines.push
      "  (only result hashes are available; see doc/quickstart.md for what to expect)"
  return "\n".intercalate lines.toList

end Format
end LeanBench
