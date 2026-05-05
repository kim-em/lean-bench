import BenchDocs.Block.Normalise

open BenchDocs.Block

/-! Test suite for `BenchDocs.Block.Normalise`.

Each case is a `(name, input, expected)` triple. Failures print to stderr
and exit nonzero so this exe can run as a test target. -/

structure Case where
  name : String
  input : String
  expected : String

private def testTiming : Array Case := #[
  ⟨"plain ns", "987 ns", "<TIME>"⟩,
  ⟨"micro µs", "197.481 µs", "<TIME>"⟩,
  ⟨"micro us", "197.481 us", "<TIME>"⟩,
  ⟨"ms", "1.23 ms", "<TIME>"⟩,
  ⟨"plain s", "12.5 s", "<TIME>"⟩,
  ⟨"with leading space", "       32_768  197.481 µs",
   "       32_768  <TIME>"⟩,
  ⟨"non-timing letter follows unit",
   "12 ms is the magic number", "<TIME> is the magic number"⟩,
  ⟨"bare number", "32_768", "32_768"⟩,
  ⟨"non-number near unit", "ms", "ms"⟩
]

private def testRatio : Array Case := #[
  ⟨"C= padded", "C= 6.027", "C=<NUM>"⟩,
  ⟨"C= negative", "C=-0.005", "C=<NUM>"⟩,
  ⟨"β=", "β=-0.005", "β=<NUM>"⟩,
  ⟨"cMin/cMax", "(cMin=5.702, cMax=6.863)", "(cMin=<NUM>, cMax=<NUM>)"⟩,
  ⟨"×2 repeat", "×2^9", "×2^<N>"⟩,
  ⟨"full verdict line",
   "verdict: consistent (cMin=5.702, cMax=6.863, β=-0.005)",
   "verdict: consistent (cMin=<NUM>, cMax=<NUM>, β=<NUM>)"⟩
]

private def testEnvJson : Array Case := #[
  ⟨"hostname spaced", "\"hostname\": \"machine.local\"",
   "\"hostname\": \"<REDACTED>\""⟩,
  ⟨"hostname nospace", "\"hostname\":\"machine.local\"",
   "\"hostname\": \"<REDACTED>\""⟩,
  ⟨"started_at", "\"started_at\": \"2024-01-01T00:00:00Z\"",
   "\"started_at\": \"<REDACTED>\""⟩,
  ⟨"unrelated field stays",
   "\"function\": \"foo\", \"hostname\": \"x\"",
   "\"function\": \"foo\", \"hostname\": \"<REDACTED>\""⟩
]

private def testSkipBlank : Array (String × String × Bool) := #[
  ("blank skipped", "", false),
  ("whitespace skipped", "   \t  ", false),
  ("non-blank kept", "foo", true)
]

def runCases (label : String) (norm : Normaliser) (cases : Array Case) : IO Nat := do
  let mut failures : Nat := 0
  for c in cases do
    let got := norm.preEq c.input
    if got != c.expected then
      failures := failures + 1
      IO.eprintln s!"  ✘ [{label}] {c.name}\n      input    = {repr c.input}\n      expected = {repr c.expected}\n      got      = {repr got}"
    else
      IO.println s!"  ✔ [{label}] {c.name}"
  return failures

def runUseLine : IO Nat := do
  let mut failures : Nat := 0
  for (name, input, expected) in testSkipBlank do
    let got := Normaliser.skipBlank.useLine input
    if got != expected then
      failures := failures + 1
      IO.eprintln s!"  ✘ [skipBlank.useLine] {name}\n      input = {repr input}\n      expected useLine = {expected}, got = {got}"
    else
      IO.println s!"  ✔ [skipBlank.useLine] {name}"
  return failures

def main : IO UInt32 := do
  let mut total : Nat := 0
  total := total + (← runCases "timing"  .timing  testTiming)
  total := total + (← runCases "ratio"   .ratio   testRatio)
  total := total + (← runCases "envJson" .envJson testEnvJson)
  total := total + (← runUseLine)
  if total == 0 then
    IO.println "all normaliser cases passed"
    return 0
  else
    IO.eprintln s!"{total} failure(s)"
    return 1
