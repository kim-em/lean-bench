import LeanBench

/-!
# Tests for issue #5 — config ergonomics

Two layers of config plumbing live in this PR:

1. `setup_benchmark … where { … }` — declaration-time overrides on
   `BenchmarkConfig`. Verified by registering benchmarks with various
   `where` clauses and reading the runtime registry back.
2. `ConfigOverride` + `ConfigOverride.apply` — what the CLI builds from
   `--max-seconds-per-call` etc., applied on top of the declared config.
   Verified by elaboration-time `example` blocks (so failures stop the
   build).

Both are unit-testable without spawning a subprocess.
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
  IO.println "config tests OK"
  return 0
