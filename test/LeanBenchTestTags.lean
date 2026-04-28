import LeanBench

/-!
# Tests for issue #10 — benchmark organization and selection

Exercises the tag and filtering infrastructure added in issue #10:

1. Tags declared via `where { tags := #[...] }` are stored on the
   runtime registry entry and recovered at runtime.
2. The CLI filtering helpers (`matchEntry`, `splitTags`) produce the
   expected results.
3. The `list` subcommand respects `--tag` and `--filter` flags.
4. The `run` subcommand dispatches on `--tag` / `--filter` when no
   positional names are given.
-/

open LeanBench

namespace LeanBench.Test.Tags

/-! ## Tagged benchmarks -/

def linearFn (n : Nat) : Nat := n + 1
setup_benchmark linearFn n => n where {
  tags := #["linear", "fast"]
}

def quadraticFn (n : Nat) : Nat := n * n
setup_benchmark quadraticFn n => n * n where {
  tags := #["quadratic"]
}

def untaggedFn (n : Nat) : Nat := n + 2
setup_benchmark untaggedFn n => n

/-! ## Helpers -/

private def expectEq {α} [BEq α] [Repr α] (label : String) (got want : α) :
    IO Unit := do
  unless got == want do
    throw <| .userError s!"{label}: expected {repr want}, got {repr got}"

private def fullName (suffix : String) : Lean.Name :=
  `LeanBench.Test.Tags ++ Lean.Name.mkSimple suffix

private def fetchTags (suffix : String) : IO (Array String) := do
  match ← LeanBench.findRuntimeEntry (fullName suffix) with
  | some e => return e.spec.config.tags
  | none => throw <| .userError s!"unregistered: {suffix}"

/-! ## Tag storage tests -/

def testTagsStored : IO UInt32 := do
  let tags ← fetchTags "linearFn"
  expectEq "linearFn.tags" tags #["linear", "fast"]
  let tags2 ← fetchTags "quadraticFn"
  expectEq "quadraticFn.tags" tags2 #["quadratic"]
  let tags3 ← fetchTags "untaggedFn"
  expectEq "untaggedFn.tags" tags3 (#[] : Array String)
  IO.println "  ok  tags.stored"
  return 0

/-! ## CLI filter parsing tests -/

/-- `--tag sort,fast` parses into individual tags. -/
def testSplitTags : IO UInt32 := do
  let parsed := LeanBench.Cli.splitTags "sort,fast"
  expectEq "splitTags.basic" parsed #["sort", "fast"]
  let single := LeanBench.Cli.splitTags "solo"
  expectEq "splitTags.single" single #["solo"]
  let empty := LeanBench.Cli.splitTags ""
  expectEq "splitTags.empty" empty (#[] : Array String)
  -- Extra commas are filtered.
  let sparse := LeanBench.Cli.splitTags "a,,b,"
  expectEq "splitTags.sparse" sparse #["a", "b"]
  IO.println "  ok  tags.splitTags"
  return 0

/-- `matchEntry` applies tag and name filters correctly. -/
def testMatchEntry : IO UInt32 := do
  let name : Lean.Name := `Foo.Bar.baz
  let tags : Array String := #["fast", "sort"]
  -- No filters → matches.
  expectEq "match.noFilter"
    (LeanBench.Cli.matchEntry name tags #[] none) true
  -- Tag filter with matching tag → matches.
  expectEq "match.tagHit"
    (LeanBench.Cli.matchEntry name tags #["sort"] none) true
  -- Tag filter with non-matching tag → no match.
  expectEq "match.tagMiss"
    (LeanBench.Cli.matchEntry name tags #["slow"] none) false
  -- Name filter with matching substring → matches.
  expectEq "match.nameHit"
    (LeanBench.Cli.matchEntry name tags #[] (some "Bar")) true
  -- Name filter with non-matching substring → no match.
  expectEq "match.nameMiss"
    (LeanBench.Cli.matchEntry name tags #[] (some "Quux")) false
  -- Both filters: tag matches, name matches → matches (AND).
  expectEq "match.bothHit"
    (LeanBench.Cli.matchEntry name tags #["fast"] (some "baz")) true
  -- Both filters: tag matches, name doesn't → no match.
  expectEq "match.tagHitNameMiss"
    (LeanBench.Cli.matchEntry name tags #["fast"] (some "Quux")) false
  -- Untagged entry vs tag filter → no match.
  expectEq "match.untaggedVsTag"
    (LeanBench.Cli.matchEntry name #[] #["fast"] none) false
  IO.println "  ok  tags.matchEntry"
  return 0

/-! ## CLI dispatch tests -/

/-- `list --tag linear` shows only the tagged benchmark. -/
def testListWithTag : IO UInt32 := do
  let code ← LeanBench.Cli.dispatch ["list", "--tag", "linear"]
  if code != 0 then
    IO.eprintln "FAIL: list --tag linear returned non-zero"
    return 1
  IO.println "  ok  tags.listWithTag"
  return 0

/-- `list --filter quadratic` shows only the matching benchmark. -/
def testListWithFilter : IO UInt32 := do
  let code ← LeanBench.Cli.dispatch ["list", "--filter", "quadratic"]
  if code != 0 then
    IO.eprintln "FAIL: list --filter quadratic returned non-zero"
    return 1
  IO.println "  ok  tags.listWithFilter"
  return 0

/-- `list --tag nonexistent` shows the "no matches" message and exits 0. -/
def testListNoMatches : IO UInt32 := do
  let code ← LeanBench.Cli.dispatch ["list", "--tag", "nonexistent"]
  if code != 0 then
    IO.eprintln "FAIL: list --tag nonexistent returned non-zero"
    return 1
  IO.println "  ok  tags.listNoMatches"
  return 0

/-- `run --tag nonexistent` exits 1 (no matches to run). -/
def testRunNoMatches : IO UInt32 := do
  let code ← LeanBench.Cli.dispatch ["run", "--tag", "nonexistent"]
  if code != 1 then
    IO.eprintln s!"FAIL: run --tag nonexistent: expected exit 1, got {code}"
    return 1
  IO.println "  ok  tags.runNoMatches"
  return 0

/-- `run` with no args and no filters exits 1 with a helpful message. -/
def testRunNoArgsNoFilters : IO UInt32 := do
  let code ← LeanBench.Cli.dispatch ["run"]
  if code != 1 then
    IO.eprintln s!"FAIL: run (bare): expected exit 1, got {code}"
    return 1
  IO.println "  ok  tags.runNoArgsNoFilters"
  return 0

/-! ## Format tests -/

/-- `fmtSpec` includes tags when present, omits when empty. -/
def testFmtSpecTags : IO UInt32 := do
  let tagged : BenchmarkSpec :=
    { name := `foo
      complexityName := `foo_c
      complexityFormula := "n"
      runCheckedName := `foo_r
      hashable := true
      config := { tags := #["sort", "fast"] } }
  let s := Format.fmtSpec tagged
  unless LeanBench.Cli.containsSub s "[sort, fast]" do
    throw <| .userError s!"expected tags in fmtSpec output: {s}"
  let untagged : BenchmarkSpec :=
    { name := `bar
      complexityName := `bar_c
      complexityFormula := "n"
      runCheckedName := `bar_r
      hashable := true
      config := {} }
  let s2 := Format.fmtSpec untagged
  if LeanBench.Cli.containsSub s2 "[" then
    throw <| .userError s!"expected no brackets in untagged fmtSpec output: {s2}"
  IO.println "  ok  tags.fmtSpecTags"
  return 0

end LeanBench.Test.Tags

/-! ## Driver -/

open LeanBench.Test.Tags in
def main : IO UInt32 := do
  let mut anyFail : UInt32 := 0
  for (label, t) in
    [ ("tags.stored",           testTagsStored),
      ("tags.splitTags",        testSplitTags),
      ("tags.matchEntry",       testMatchEntry),
      ("tags.listWithTag",      testListWithTag),
      ("tags.listWithFilter",   testListWithFilter),
      ("tags.listNoMatches",    testListNoMatches),
      ("tags.runNoMatches",     testRunNoMatches),
      ("tags.runNoArgsNoFilters", testRunNoArgsNoFilters),
      ("tags.fmtSpecTags",      testFmtSpecTags) ]
  do
    let code ← t
    if code != 0 then
      IO.eprintln s!"FAIL: {label}"
      anyFail := 1
  if anyFail == 0 then
    IO.println "tags tests passed"
  return anyFail
