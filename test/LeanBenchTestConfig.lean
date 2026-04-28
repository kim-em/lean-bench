import LeanBench

/-!
# Tests for issue #5 / #28 — config ergonomics

Three layers of config plumbing are exercised here:

1. `setup_benchmark … where { … }` — declaration-time overrides on
   `BenchmarkConfig`. Verified by registering benchmarks with various
   `where` clauses and reading the runtime registry back.
2. `ConfigOverride` + `ConfigOverride.apply` — what the CLI builds from
   `--max-seconds-per-call` etc., applied on top of the declared config.
   Verified by elaboration-time `example` blocks (so failures stop the
   build).
3. CLI argv parsing — drive the `run` subcommand's parser with a
   handcrafted argv list and verify `configOverrideFromParsed`
   recovers each override flag with the right type. This is the
   "tests cover CLI behavior" leg of the issue #28 acceptance
   criteria; the previous test suite only covered parsing-free
   `ConfigOverride.apply` semantics.

Everything is unit-testable without spawning a subprocess.
-/

open LeanBench

namespace LeanBench.Test.Config

/-! ## Benchmarks declared with various `where { … }` clauses. -/

def trivialOverridden (n : Nat) : Nat := n + 1
setup_benchmark trivialOverridden n => n where {
  maxSecondsPerCall := 0.25
  paramCeiling := 256
  verdictWarmupFraction := 0.4
  slopeTolerance := 0.3
}

def trivialDefault (n : Nat) : Nat := n + 2
setup_benchmark trivialDefault n => n

def trivialPartial (n : Nat) : Nat := n + 3
setup_benchmark trivialPartial n => n where { paramFloor := 16 }

/-! ## `ConfigOverride.apply` layered on top of declared config (compile-time). -/

/-- Empty override leaves the declared config untouched. -/
def cfg₁ : BenchmarkConfig := { maxSecondsPerCall := 2.0, paramCeiling := 64 }
example : (({} : ConfigOverride).apply cfg₁).maxSecondsPerCall = 2.0 := rfl
example : (({} : ConfigOverride).apply cfg₁).paramCeiling = 64 := rfl

/-- Some-fields override only those fields. -/
def ovr₁ : ConfigOverride :=
  { maxSecondsPerCall? := some 0.5, paramCeiling? := some 1024 }
example : (ovr₁.apply cfg₁).maxSecondsPerCall = 0.5 := rfl
example : (ovr₁.apply cfg₁).paramCeiling = 1024 := rfl
example : (ovr₁.apply cfg₁).targetInnerNanos = cfg₁.targetInnerNanos := rfl

/-- All-fields override replaces every field. -/
def fullOvr : ConfigOverride :=
  { targetInnerNanos? := some 1, maxSecondsPerCall? := some 2.0
    paramCeiling? := some 3, paramFloor? := some 4
    verdictWarmupFraction? := some 0.5, slopeTolerance? := some 0.6
    killGraceMs? := some 7 }
example : (fullOvr.apply ({} : BenchmarkConfig)).targetInnerNanos = 1 := rfl
example : (fullOvr.apply ({} : BenchmarkConfig)).paramFloor = 4 := rfl

/-! ## Layering precedence: CLI overrides win, untouched fields stay declared. -/

/-- A declared config (e.g. via `setup_benchmark … where { … }`)
overlaid with a partial CLI override applies only the CLI fields and
leaves the rest of the declared config alone. -/
def declaredCfg : BenchmarkConfig :=
  { maxSecondsPerCall := 5.0
    paramCeiling := 1024
    verdictWarmupFraction := 0.4
    slopeTolerance := 0.3 }
def cliOvr : ConfigOverride :=
  { paramCeiling? := some 64, slopeTolerance? := some 0.05 }
example : (cliOvr.apply declaredCfg).maxSecondsPerCall = 5.0 := rfl
example : (cliOvr.apply declaredCfg).paramCeiling = 64 := rfl

/-! ## `BenchmarkConfig.validate` rejects pathological configs.

Float comparisons don't reduce kernel-side so we use `native_decide`
for the float-bearing cases. -/

example : (BenchmarkConfig.validate { maxSecondsPerCall := -1.0 }).isOk = false := by native_decide
example : (BenchmarkConfig.validate { targetInnerNanos := 0 }).isOk = false := by native_decide
example : (BenchmarkConfig.validate { slopeTolerance := -0.1 }).isOk = false := by native_decide
example : (BenchmarkConfig.validate { verdictWarmupFraction := 1.0 }).isOk = false := by native_decide
example : (BenchmarkConfig.validate { paramFloor := 100, paramCeiling := 10 }).isOk = false := by native_decide
example : (BenchmarkConfig.validate ({} : BenchmarkConfig)).isOk = true := by native_decide

end LeanBench.Test.Config

/-! ## Runtime check: registered benchmarks carry the configured override. -/

private def fullName (suffix : String) : Lean.Name :=
  -- `setup_benchmark` registers hierarchical names (via `quote fnName`),
  -- so build the matching shape: `LeanBench`, `Test`, `Config`, suffix.
  `LeanBench.Test.Config ++ Lean.Name.mkSimple suffix

private def fetchConfig (suffix : String) : IO BenchmarkConfig := do
  match ← LeanBench.findRuntimeEntry (fullName suffix) with
  | some e => return e.spec.config
  | none => throw <| .userError s!"unregistered: {suffix}"

private def expectEq {α} [BEq α] [Repr α] (label : String) (got want : α) :
    IO Unit := do
  unless got == want do
    throw <| .userError
      s!"{label}: expected {repr want}, got {repr got}"

/-! ## End-to-end CLI argv parsing — drive the actual `Cli` command tree.

`Cli.Cmd.process` returns `(matched-Cmd × Parsed)` without dispatching
the runner, so we can hand it a `run NAME --flag value …` argv,
extract the `Parsed`, and assert `configOverrideFromParsed` recovers
each field with the right type. This is a real test of the
parser-to-`ConfigOverride` pipeline rather than a hand-built
`ConfigOverride` literal. -/

/-- Drive `topCmd.process` with a concrete argv list and either
return the `Parsed` from the matched subcommand or fail the test
with a descriptive error. -/
private def parseRunArgs (argv : List String) : IO Cli.Parsed := do
  match LeanBench.Cli.topCmd.process argv with
  | .ok (_, parsed) => return parsed
  | .error (_, msg) =>
    throw <| .userError s!"unexpected parse failure for argv {argv}: {msg}"

/-- Every parametric override flag round-trips through the CLI parser
into `ConfigOverride` with the right type. The `repeats` flag is
fixed-only and ignored on the parametric side; we still assert that
mixing parametric and fixed flags doesn't fail the parser, since
`run` accepts both. -/
def testCliParsesAllOverrides : IO UInt32 := do
  let argv := [
    "run", "trivialDefault",
    "--max-seconds-per-call", "0.5",
    "--target-inner-nanos", "250000000",
    "--param-floor", "8",
    "--param-ceiling", "1024",
    "--warmup-fraction", "0.3",
    "--slope-tolerance", "0.05",
    "--param-schedule", "linear" ]
  let parsed ← parseRunArgs argv
  let ovr := LeanBench.Cli.configOverrideFromParsed parsed
  expectEq "cli.maxSecondsPerCall"     ovr.maxSecondsPerCall?     (some 0.5)
  expectEq "cli.targetInnerNanos"      ovr.targetInnerNanos?      (some 250_000_000)
  expectEq "cli.paramFloor"            ovr.paramFloor?            (some 8)
  expectEq "cli.paramCeiling"          ovr.paramCeiling?          (some 1024)
  expectEq "cli.verdictWarmupFraction" ovr.verdictWarmupFraction? (some 0.3)
  expectEq "cli.slopeTolerance"        ovr.slopeTolerance?        (some 0.05)
  expectEq "cli.paramSchedule"         ovr.paramSchedule?         (some .linear)
  return 0

/-- Missing flags leave the corresponding `ConfigOverride` fields
`none`, so the declared config bubbles through `apply`. -/
def testCliMissingFlagsAreNone : IO UInt32 := do
  let parsed ← parseRunArgs ["run", "trivialDefault"]
  let ovr := LeanBench.Cli.configOverrideFromParsed parsed
  -- Every override field must default to `none` when no flag was passed.
  expectEq "missing.maxSecondsPerCall"     ovr.maxSecondsPerCall?     (none : Option Float)
  expectEq "missing.targetInnerNanos"      ovr.targetInnerNanos?      (none : Option Nat)
  expectEq "missing.paramCeiling"          ovr.paramCeiling?          (none : Option Nat)
  expectEq "missing.paramFloor"            ovr.paramFloor?            (none : Option Nat)
  expectEq "missing.verdictWarmupFraction" ovr.verdictWarmupFraction? (none : Option Float)
  expectEq "missing.slopeTolerance"        ovr.slopeTolerance?        (none : Option Float)
  expectEq "missing.paramSchedule"         ovr.paramSchedule?         (none : Option ParamSchedule)
  -- And the resulting config equals the declared config exactly.
  let declared : BenchmarkConfig :=
    { maxSecondsPerCall := 0.25, paramCeiling := 256
      verdictWarmupFraction := 0.4, slopeTolerance := 0.3 }
  expectEq "applied config equals declared on empty CLI"
    (ovr.apply declared) declared
  return 0

/-- The fixed-only `--repeats` flag is recognised on `run` and feeds
`fixedConfigOverrideFromParsed`. The parametric override builder
ignores it, the fixed one picks it up — both assertions matter. -/
def testCliRepeatsIsFixedOnly : IO UInt32 := do
  let parsed ← parseRunArgs
    ["run", "trivialDefault", "--repeats", "7", "--max-seconds-per-call", "1.5"]
  let pOvr := LeanBench.Cli.configOverrideFromParsed parsed
  let fOvr := LeanBench.Cli.fixedConfigOverrideFromParsed parsed
  expectEq "fixed.repeats"           fOvr.repeats?           (some 7)
  expectEq "fixed.maxSecondsPerCall" fOvr.maxSecondsPerCall? (some 1.5)
  -- The shared flag also lands on the parametric override.
  expectEq "shared.maxSecondsPerCall" pOvr.maxSecondsPerCall? (some 1.5)
  return 0

/-- `--param-schedule auto|doubling|linear` parses every documented
spelling. Case-insensitivity is part of the contract; the matching
`Cli.ParseableType` instance lowercases before comparing. -/
def testCliParamScheduleSpellings : IO UInt32 := do
  let cases : List (String × ParamSchedule) :=
    [("auto", .auto), ("doubling", .doubling), ("linear", .linear),
     ("AUTO", .auto), ("Linear", .linear)]
  for (s, expected) in cases do
    let parsed ← parseRunArgs ["run", "trivialDefault", "--param-schedule", s]
    let ovr := LeanBench.Cli.configOverrideFromParsed parsed
    unless ovr.paramSchedule? == some expected do
      throw <| .userError
        s!"--param-schedule {s}: expected {repr (some expected)}, got {repr ovr.paramSchedule?}"
  -- An unknown spelling must be rejected by the parser, not silently
  -- dropped to `none` (which would make typos look like "use the
  -- declared default"). A failure here means the `parse?` instance
  -- accepted garbage.
  match LeanBench.Cli.topCmd.process
      ["run", "trivialDefault", "--param-schedule", "bogus"] with
  | .ok _ =>
    throw <| .userError "expected --param-schedule bogus to be rejected"
  | .error _ => return 0

/-- The full pipeline: a parsed CLI override layered on top of a
declared `where { … }` config produces the expected merged config.
Pins that `paramSchedule` propagates from CLI as well as from the
declared `where` clause, and that an unrecognised long flag is
rejected by the parser (no silent typo fallthrough). -/
def testCliLayersOnDeclaredConfig : IO UInt32 := do
  -- Typo rejection: an unknown long flag must fail parsing rather
  -- than be silently dropped, otherwise `--paramceiling 512` would
  -- look like "use the declared default" and confuse the user.
  match LeanBench.Cli.topCmd.process
      ["run", "trivialDefault", "--paramceiling-not-real", "512"] with
  | .ok _ =>
    throw <| .userError "expected unknown flag to be rejected by parser"
  | .error _ => pure ()
  -- Real merge: CLI override layers on top of the declared
  -- `where { ... }` config. Untouched declared fields survive.
  let parsed ← parseRunArgs
    ["run", "trivialOverridden",
     "--param-ceiling", "32",
     "--param-schedule", "doubling"]
  let ovr := LeanBench.Cli.configOverrideFromParsed parsed
  let declared ← fetchConfig "trivialOverridden"
  let merged := ovr.apply declared
  -- CLI overrides win where they were passed.
  expectEq "merged.paramCeiling" merged.paramCeiling 32
  expectEq "merged.paramSchedule" merged.paramSchedule .doubling
  -- Untouched declared fields survive.
  expectEq "merged.maxSecondsPerCall" merged.maxSecondsPerCall 0.25
  expectEq "merged.verdictWarmupFraction" merged.verdictWarmupFraction 0.4
  expectEq "merged.slopeTolerance" merged.slopeTolerance 0.3
  return 0

/-- The `compare` subcommand carries its own flag table — adding
`--param-schedule` to `run` only is a footgun, so this test
parses the same argv against `compare` and expects an identical
override. A regression here would be a copy-paste miss between the
two `[Cli|]` blocks in `LeanBench.Cli`. -/
def testCliCompareAcceptsAllOverrides : IO UInt32 := do
  -- Pass two benchmark names so `compare` accepts the variadic
  -- positional args; the override flag set must round-trip the same
  -- way as `run`.
  let argv := [
    "compare", "trivialDefault", "trivialOverridden",
    "--max-seconds-per-call", "0.5",
    "--target-inner-nanos", "250000000",
    "--param-floor", "8",
    "--param-ceiling", "1024",
    "--warmup-fraction", "0.3",
    "--slope-tolerance", "0.05",
    "--param-schedule", "doubling" ]
  let parsed ← parseRunArgs argv
  let ovr := LeanBench.Cli.configOverrideFromParsed parsed
  expectEq "compare.maxSecondsPerCall"     ovr.maxSecondsPerCall?     (some 0.5)
  expectEq "compare.targetInnerNanos"      ovr.targetInnerNanos?      (some 250_000_000)
  expectEq "compare.paramFloor"            ovr.paramFloor?            (some 8)
  expectEq "compare.paramCeiling"          ovr.paramCeiling?          (some 1024)
  expectEq "compare.verdictWarmupFraction" ovr.verdictWarmupFraction? (some 0.3)
  expectEq "compare.slopeTolerance"        ovr.slopeTolerance?        (some 0.05)
  expectEq "compare.paramSchedule"         ovr.paramSchedule?         (some .doubling)
  -- And bogus values get rejected on `compare` too.
  match LeanBench.Cli.topCmd.process
      ["compare", "trivialDefault", "trivialOverridden",
       "--param-schedule", "bogus"] with
  | .ok _ =>
    throw <| .userError "expected `compare --param-schedule bogus` to be rejected"
  | .error _ => return 0

/-- `mergeParamSchedule` semantics:
    - CLI `.linear _` on a declared `.linear n` carries `n` forward
      (the CLI flag has no surface for the sample count, so it
      acts as a no-op when the shape is already linear).
    - CLI shape switch (e.g. declared `.doubling`, CLI `.linear`)
      lands on the macro-default sample count (16).
    - CLI `.doubling` / `.auto` overwrites declared as expected.
    - No CLI override (`none`) preserves declared exactly. -/
def testParamScheduleMerge : IO UInt32 := do
  let baseLinear32 : BenchmarkConfig := { paramSchedule := .linear 32 }
  let baseDoubling : BenchmarkConfig := { paramSchedule := .doubling }
  let cliLinear : ConfigOverride := { paramSchedule? := some .linear }
  let cliDoubling : ConfigOverride := { paramSchedule? := some .doubling }
  let cliAuto : ConfigOverride := { paramSchedule? := some .auto }
  let empty : ConfigOverride := {}
  -- `--param-schedule linear` on declared `.linear 32` keeps `32`.
  expectEq "merge.linearOnLinear32"
    (cliLinear.apply baseLinear32).paramSchedule (.linear 32)
  -- `--param-schedule linear` on declared `.doubling` lands on default samples.
  expectEq "merge.linearOnDoubling"
    (cliLinear.apply baseDoubling).paramSchedule (.linear)
  -- `--param-schedule doubling` overwrites declared `.linear 32`.
  expectEq "merge.doublingOnLinear32"
    (cliDoubling.apply baseLinear32).paramSchedule .doubling
  -- `--param-schedule auto` overwrites declared.
  expectEq "merge.autoOnLinear32"
    (cliAuto.apply baseLinear32).paramSchedule .auto
  -- Empty CLI override preserves declared exactly.
  expectEq "merge.emptyKeepsLinear32"
    (empty.apply baseLinear32).paramSchedule (.linear 32)
  expectEq "merge.emptyKeepsDoubling"
    (empty.apply baseDoubling).paramSchedule .doubling
  return 0

/-- Drive every CLI test fail-fast. Returns the first non-zero exit
code; on success, a single confirmation line. -/
private def runCliTests : IO UInt32 := do
  for (label, t) in
    [("cli.parsesAllOverrides", testCliParsesAllOverrides),
     ("cli.compareAcceptsAllOverrides", testCliCompareAcceptsAllOverrides),
     ("cli.missingFlagsAreNone", testCliMissingFlagsAreNone),
     ("cli.repeatsIsFixedOnly", testCliRepeatsIsFixedOnly),
     ("cli.paramScheduleSpellings", testCliParamScheduleSpellings),
     ("cli.layersOnDeclaredConfig", testCliLayersOnDeclaredConfig),
     ("cli.paramScheduleMerge", testParamScheduleMerge)]
  do
    let code ← t
    if code != 0 then
      IO.eprintln s!"FAIL: {label}"
      return code
    IO.println s!"  ok  {label}"
  return 0

def main : IO UInt32 := do
  -- `trivialOverridden` carries every override we set.
  let c ← fetchConfig "trivialOverridden"
  expectEq "overridden.maxSecondsPerCall"     c.maxSecondsPerCall 0.25
  expectEq "overridden.paramCeiling"          c.paramCeiling 256
  expectEq "overridden.verdictWarmupFraction" c.verdictWarmupFraction 0.4
  expectEq "overridden.slopeTolerance"        c.slopeTolerance 0.3
  -- Fields not mentioned in `where` keep the structure default.
  let dflt : BenchmarkConfig := {}
  expectEq "overridden.targetInnerNanos default"
    c.targetInnerNanos dflt.targetInnerNanos
  expectEq "overridden.killGraceMs default"
    c.killGraceMs dflt.killGraceMs
  -- `trivialDefault` (no `where`) gets every default.
  let d ← fetchConfig "trivialDefault"
  expectEq "default config" d dflt
  -- `trivialPartial` overrides only `paramFloor`.
  let p ← fetchConfig "trivialPartial"
  expectEq "partial.paramFloor" p.paramFloor 16
  expectEq "partial.maxSecondsPerCall" p.maxSecondsPerCall dflt.maxSecondsPerCall
  expectEq "partial.paramCeiling" p.paramCeiling dflt.paramCeiling
  -- A bad CLI override is rejected by `runBenchmark` before any
  -- subprocesses spawn. We use a `paramFloor > paramCeiling` override
  -- so the `BenchmarkConfig.validate` call fires.
  let badOvr : ConfigOverride :=
    { paramFloor? := some 100, paramCeiling? := some 10 }
  try
    let _ ← LeanBench.runBenchmark
      (fullName "trivialDefault") badOvr
    throw <| .userError "expected validate to reject paramFloor > paramCeiling"
  catch e =>
    let msg := e.toString
    unless msg.endsWith "paramFloor must be ≤ paramCeiling; got 100 > 10" do
      throw <| .userError s!"unexpected error message: {msg}"
  -- CLI argv parsing tests.
  let cliCode ← runCliTests
  if cliCode != 0 then return cliCode
  IO.println "config tests OK"
  return 0
