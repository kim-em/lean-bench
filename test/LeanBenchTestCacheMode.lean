import LeanBench

/-!
# Cache-mode tests (issue #12)

Pin the warm/cold execution-mode plumbing end-to-end:

- `setup_benchmark … where { cacheMode := .cold }` is plumbed onto
  the runtime spec.
- `--cache-mode cold` round-trips through the CLI parser into a
  `ConfigOverride`, layers correctly on top of declared config, and
  rejects unknown spellings.
- `ConfigOverride.apply` carries the cache mode through.
- The child path actually behaves differently: warm produces an
  auto-tuned `inner_repeats > 1`, cold produces `inner_repeats = 1`.
- The emitted JSONL row carries the `cache_mode` discriminator string
  on the parametric path.
- Older fixtures missing `cache_mode` parse without error
  (back-compat: it's an additive optional field).

The same compiled binary acts as parent (the test driver) and child
(when the parent's `runOneBatch` spawns it via `_child`).
-/

open LeanBench

namespace LeanBench.Test.CacheMode

/-- A trivially-fast `Nat → UInt64`. The benchmark is intentionally
    cheap so warm-mode autotune ramps `inner_repeats` past 1 quickly,
    making the warm-vs-cold count distinction observable in a fast
    test. -/
def cacheProbeFn (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 1
  for _ in [0:n] do x := x ^^^ n.toUInt64
  return x

setup_benchmark cacheProbeFn n => n where {
  maxSecondsPerCall := 0.5
  paramCeiling := 8
  targetInnerNanos := 50_000_000
}

/-- Same body, declared with `where { cacheMode := .cold }` so we can
    pin the declaration-time override path without going through the
    CLI. -/
def cacheProbeColdFn (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 1
  for _ in [0:n] do x := x ^^^ n.toUInt64
  return x

setup_benchmark cacheProbeColdFn n => n where {
  maxSecondsPerCall := 0.5
  paramCeiling := 4
  targetInnerNanos := 50_000_000
  cacheMode := .cold
}

end LeanBench.Test.CacheMode

private def warmName : Lean.Name := `LeanBench.Test.CacheMode.cacheProbeFn
private def coldName : Lean.Name := `LeanBench.Test.CacheMode.cacheProbeColdFn

/-- Substring containment helper. -/
private def containsSub (haystack needle : String) : Bool :=
  ((haystack.splitOn needle).length) > 1

private def expectEq {α} [BEq α] [Repr α] (label : String) (got want : α) :
    IO UInt32 := do
  unless got == want do
    IO.eprintln s!"FAIL: {label}: expected {repr want}, got {repr got}"
    return 1
  return 0

/-! ## Pure: defaults, override application, structural equalities -/

def testDefaultIsWarm : IO UInt32 := do
  let cfg : BenchmarkConfig := {}
  expectEq "default cacheMode" cfg.cacheMode .warm

def testCacheModeJsonStrings : IO UInt32 := do
  -- Pinned wire-format strings, mirrored on `Schema.cacheModeWarm`/
  -- `cacheModeCold` and consumed by external tools.
  let r1 ← expectEq "warm.toJsonString" CacheMode.warm.toJsonString "warm"
  if r1 != 0 then return r1
  let r2 ← expectEq "cold.toJsonString" CacheMode.cold.toJsonString "cold"
  if r2 != 0 then return r2
  let r3 ← expectEq "schema.warm" Schema.cacheModeWarm "warm"
  if r3 != 0 then return r3
  let r4 ← expectEq "schema.cold" Schema.cacheModeCold "cold"
  if r4 != 0 then return r4
  expectEq "schema.cacheModeStrings"
    Schema.cacheModeStrings #["warm", "cold"]

def testApplyCarriesCacheMode : IO UInt32 := do
  -- `none` keeps the declared value; `some` replaces it. Same shape
  -- as every other `ConfigOverride.apply` field — pinning it here
  -- guards against a copy-paste miss in `apply`.
  let baseWarm : BenchmarkConfig := { cacheMode := .warm }
  let baseCold : BenchmarkConfig := { cacheMode := .cold }
  let empty : ConfigOverride := {}
  let toCold : ConfigOverride := { cacheMode? := some .cold }
  let toWarm : ConfigOverride := { cacheMode? := some .warm }
  let r1 ← expectEq "empty keeps warm" (empty.apply baseWarm).cacheMode .warm
  if r1 != 0 then return r1
  let r2 ← expectEq "empty keeps cold" (empty.apply baseCold).cacheMode .cold
  if r2 != 0 then return r2
  let r3 ← expectEq "override flips warm→cold"
    (toCold.apply baseWarm).cacheMode .cold
  if r3 != 0 then return r3
  expectEq "override flips cold→warm"
    (toWarm.apply baseCold).cacheMode .warm

/-! ## Declaration-time `where { cacheMode := .cold }` plumbing -/

def testDeclaredCacheModeIsRegistered : IO UInt32 := do
  match ← findRuntimeEntry coldName with
  | none =>
    IO.eprintln s!"FAIL: registry lookup failed for {coldName}"
    return 1
  | some entry =>
    expectEq "registered cacheMode" entry.spec.config.cacheMode .cold

def testDeclaredDefaultIsWarm : IO UInt32 := do
  match ← findRuntimeEntry warmName with
  | none =>
    IO.eprintln s!"FAIL: registry lookup failed for {warmName}"
    return 1
  | some entry =>
    expectEq "registered default cacheMode" entry.spec.config.cacheMode .warm

/-! ## CLI parser round-trip -/

private def parseRunArgs (argv : List String) : IO Cli.Parsed := do
  match LeanBench.Cli.topCmd.process argv with
  | .ok (_, parsed) => return parsed
  | .error (_, msg) =>
    throw <| .userError s!"unexpected parse failure for argv {argv}: {msg}"

def testCliCacheModeRoundtrips : IO UInt32 := do
  -- Both spellings, plus a case-insensitive variant, must parse into
  -- the right `ConfigOverride`. Missing flag is `none`.
  for (s, expected) in [("warm", CacheMode.warm),
                        ("cold", CacheMode.cold),
                        ("COLD", CacheMode.cold),
                        ("Warm", CacheMode.warm)] do
    let parsed ← parseRunArgs ["run", "cacheProbeFn", "--cache-mode", s]
    let ovr := LeanBench.Cli.configOverrideFromParsed parsed
    let r ← expectEq s!"--cache-mode {s}" ovr.cacheMode? (some expected)
    if r != 0 then return r
  -- Missing flag → none, declared default propagates.
  let parsed ← parseRunArgs ["run", "cacheProbeFn"]
  let ovr := LeanBench.Cli.configOverrideFromParsed parsed
  expectEq "missing flag" ovr.cacheMode? (none : Option CacheMode)

def testCliCacheModeRejectsBogus : IO UInt32 := do
  -- An unrecognised spelling must be rejected by the parser, not
  -- silently dropped to `none` (which would look like "use the
  -- declared default" and confuse the user).
  match LeanBench.Cli.topCmd.process
      ["run", "cacheProbeFn", "--cache-mode", "lukewarm"] with
  | .ok _ =>
    IO.eprintln "FAIL: expected --cache-mode lukewarm to be rejected"
    return 1
  | .error _ => return 0

def testCliCompareAcceptsCacheMode : IO UInt32 := do
  -- The compare subcommand has its own [Cli|] flag block; the flag must
  -- appear there too, otherwise --cache-mode is silently a `run`-only
  -- knob.
  let parsed ← parseRunArgs
    ["compare", "cacheProbeFn", "cacheProbeColdFn", "--cache-mode", "cold"]
  let ovr := LeanBench.Cli.configOverrideFromParsed parsed
  expectEq "compare.cacheMode" ovr.cacheMode? (some CacheMode.cold)

/-! ## Behavioural: the child actually behaves differently in cold mode -/

/-- Spawn the cold-declared benchmark and assert the child reports
    `inner_repeats = 1`. Warm mode would auto-tune up to a power of
    two ≥ 2; this is the strongest cheap signal that the cold path
    really runs. -/
def testColdRunHasOneInnerRepeat : IO UInt32 := do
  match ← findRuntimeEntry coldName with
  | none =>
    IO.eprintln s!"FAIL: {coldName} unregistered"
    return 1
  | some entry =>
    let dp ← runOneBatch entry.spec 4
    match dp.status with
    | .ok =>
      if dp.innerRepeats == 1 then return 0
      IO.eprintln s!"FAIL: cold inner_repeats expected 1, got {dp.innerRepeats}"
      return 1
    | s =>
      IO.eprintln s!"FAIL: cold child not ok: {repr s}"
      return 1

/-- And the warm path produces `inner_repeats > 1`, so the test isn't
    accidentally passing because both paths happen to land on 1. -/
def testWarmRunAutotunes : IO UInt32 := do
  match ← findRuntimeEntry warmName with
  | none =>
    IO.eprintln s!"FAIL: {warmName} unregistered"
    return 1
  | some entry =>
    let dp ← runOneBatch entry.spec 4
    match dp.status with
    | .ok =>
      if dp.innerRepeats > 1 then return 0
      IO.eprintln s!"FAIL: warm inner_repeats expected > 1, got {dp.innerRepeats}"
      return 1
    | s =>
      IO.eprintln s!"FAIL: warm child not ok: {repr s}"
      return 1

/-- Run-time CLI override: the parametric ladder runs in cold mode
    when the spec was declared warm but the override flips it. Mirror
    of `testColdRunHasOneInnerRepeat` but going through `apply`. -/
def testCliColdOverrideTakesEffect : IO UInt32 := do
  match ← findRuntimeEntry warmName with
  | none =>
    IO.eprintln s!"FAIL: {warmName} unregistered"
    return 1
  | some entry =>
    let cfg : BenchmarkConfig :=
      ({ cacheMode? := some .cold } : ConfigOverride).apply entry.spec.config
    let spec := { entry.spec with config := cfg }
    let dp ← runOneBatch spec 4
    match dp.status with
    | .ok =>
      if dp.innerRepeats == 1 then return 0
      IO.eprintln s!"FAIL: cold-override inner_repeats expected 1, got {dp.innerRepeats}"
      return 1
    | s =>
      IO.eprintln s!"FAIL: cold-override child not ok: {repr s}"
      return 1

/-! ## Wire format: cache_mode appears on emitted rows; absence is tolerated -/

/-- Spawn the binary in `_child` mode with `--cache-mode cold` and
    inspect the raw JSONL line. The point is to pin the wire-format
    string, not the parser's view. -/
private def spawnRawChild (args : List String) : IO String := do
  let exe := (← IO.appPath).toString
  let child ← IO.Process.spawn {
    cmd := exe
    args := args.toArray
    stdout := .piped
    stderr := .piped
    stdin := .null
  }
  let stdout ← child.stdout.readToEnd
  let _ ← child.wait
  return stdout.trimAscii.toString

def testEmittedRowCarriesCacheMode : IO UInt32 := do
  for (mode, expected) in [("warm", "warm"), ("cold", "cold")] do
    let line ← spawnRawChild
      ["_child", "--bench", warmName.toString (escape := false),
       "--param", "2", "--target-nanos", "1000000",
       "--cache-mode", mode]
    match Lean.Json.parse line with
    | .error e =>
      IO.eprintln s!"FAIL: row failed to parse: {e}; line: {line}"
      return 1
    | .ok j =>
      match j.getObjValAs? String "cache_mode" with
      | .ok s =>
        unless s == expected do
          IO.eprintln s!"FAIL: expected cache_mode={expected}, got {s}"
          return 1
      | .error _ =>
        IO.eprintln s!"FAIL: row missing cache_mode field; line: {line}"
        return 1
  return 0

/-! ## Edge interactions: kill cap + linear schedule -/

namespace LeanBench.Test.CacheMode

/-- Deliberately-slow benchmark to force the kill-on-cap path under
    cold mode. The body burns enough CPU at `param ≥ 256` that a
    single call exceeds 100ms; with `maxSecondsPerCall := 0` the
    parent fires SIGTERM ~immediately. -/
def coldKillFn (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 0
  for _ in [0:n] do
    for _ in [0:100_000] do
      x := x ^^^ n.toUInt64
  return x

setup_benchmark coldKillFn n => n where { cacheMode := .cold }

/-- Linear-schedule cold-mode benchmark. Tiny work so the doubling
    probe walks past `paramCeiling` cleanly and we can observe sweep
    rows landing on `inner_repeats = 1` even under the linear
    bracket. -/
def coldLinearFn (n : Nat) : UInt64 := Id.run do
  let mut x : UInt64 := 1
  for _ in [0:n] do x := x ^^^ n.toUInt64
  return x

setup_benchmark coldLinearFn n => n where {
  cacheMode := .cold
  paramSchedule := .linear 4
  paramCeiling := 16
  maxSecondsPerCall := 0.5
  targetInnerNanos := 50_000_000
}

end LeanBench.Test.CacheMode

private def coldKillName : Lean.Name := `LeanBench.Test.CacheMode.coldKillFn
private def coldLinearName : Lean.Name := `LeanBench.Test.CacheMode.coldLinearFn

/-- The kill-on-cap path applies in cold mode too: the parent's
    deadline is mode-agnostic, so a slow function under
    `--cache-mode cold` with `maxSecondsPerCall := 0` still synthesises
    a `killedAtCap` row rather than hanging. -/
def testColdKillPathFires : IO UInt32 := do
  match ← findRuntimeEntry coldKillName with
  | none =>
    IO.eprintln s!"FAIL: {coldKillName} unregistered"
    return 1
  | some entry =>
    let tightCfg : BenchmarkConfig :=
      { entry.spec.config with
          maxSecondsPerCall := 0
          killGraceMs := 100 }
    let dp ← runOneBatch { entry.spec with config := tightCfg } 1_000_000
    match dp.status with
    | .killedAtCap => return 0
    | s =>
      IO.eprintln s!"FAIL: cold-mode kill expected killedAtCap, got {repr s}"
      return 1

/-- Cold mode under the linear schedule: every sweep row must still
    land on `inner_repeats = 1`. Probe rows from the doubling probe
    keep `partOfVerdict = false`; sweep rows keep `partOfVerdict =
    true` and `inner_repeats = 1`. We don't pin which rungs land in
    which bracket (that's the schedule's job, not the cache mode's),
    only that no row escapes the cold path's "single invocation"
    contract. -/
def testColdUnderLinearScheduleSingleInvocation : IO UInt32 := do
  let result ← runBenchmark coldLinearName
  if result.points.isEmpty then
    IO.eprintln "FAIL: cold+linear produced no points"
    return 1
  for dp in result.points do
    -- Error / killed rows have inner_repeats = 0; ok rows MUST be 1.
    if dp.status == .ok && dp.innerRepeats != 1 then
      IO.eprintln s!"FAIL: cold+linear ok row has inner_repeats={dp.innerRepeats}; expected 1; row={repr dp}"
      return 1
  -- Sanity check: at least one ok row must be present, otherwise the
  -- assertion above is vacuous.
  let okCount := result.points.filter (·.status == .ok) |>.size
  if okCount == 0 then
    IO.eprintln s!"FAIL: cold+linear had no ok rows; points={repr result.points}"
    return 1
  return 0

def testParseToleratesMissingCacheMode : IO UInt32 := do
  -- Hand-rolled fixtures and pre-issue-12 rows omit `cache_mode`.
  -- The parser must still succeed (additive optional field).
  let row :=
    "{\"schema_version\":1,\"function\":\"foo.bar\"," ++
    "\"param\":7,\"inner_repeats\":2,\"total_nanos\":1000," ++
    "\"per_call_nanos\":500.0,\"status\":\"ok\"}"
  match parseChildRow row with
  | .error e =>
    IO.eprintln s!"FAIL: missing cache_mode row failed to parse: {e}"
    return 1
  | .ok dp =>
    expectEq "missing-cache-mode parse" dp.status .ok

/-! ## Driver -/

def runTests : IO UInt32 := do
  let mut anyFail : UInt32 := 0
  for (label, t) in
    [ ("default.isWarm",                   testDefaultIsWarm),
      ("jsonStrings",                      testCacheModeJsonStrings),
      ("apply.carriesCacheMode",           testApplyCarriesCacheMode),
      ("declared.coldRegistered",          testDeclaredCacheModeIsRegistered),
      ("declared.defaultWarm",             testDeclaredDefaultIsWarm),
      ("cli.cacheModeRoundtrips",          testCliCacheModeRoundtrips),
      ("cli.cacheModeRejectsBogus",        testCliCacheModeRejectsBogus),
      ("cli.compareAcceptsCacheMode",      testCliCompareAcceptsCacheMode),
      ("behaviour.coldOneInnerRepeat",     testColdRunHasOneInnerRepeat),
      ("behaviour.warmAutotunes",          testWarmRunAutotunes),
      ("behaviour.cliColdOverride",        testCliColdOverrideTakesEffect),
      ("edges.coldKillPathFires",          testColdKillPathFires),
      ("edges.coldLinearSingleInvocation", testColdUnderLinearScheduleSingleInvocation),
      ("wire.rowCarriesCacheMode",         testEmittedRowCarriesCacheMode),
      ("wire.parseToleratesMissing",       testParseToleratesMissingCacheMode) ]
  do
    let code ← t
    if code != 0 then
      IO.eprintln s!"FAIL: {label}"
      anyFail := 1
    else
      IO.println s!"  ok  {label}"
  if anyFail == 0 then
    IO.println "cache-mode tests passed"
  return anyFail

/-- The same compiled binary acts as parent (test driver) and child
    (single-batch runner spawned by `runOneBatch` and the raw
    `spawnRawChild` fixture). The `_child` first arg distinguishes
    them. -/
def main (args : List String) : IO UInt32 :=
  match args with
  | "_child" :: _ => LeanBench.Cli.dispatch args
  | _ => runTests
