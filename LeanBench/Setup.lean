import Lean
import LeanBench.Core
import LeanBench.Env

/-!
# `LeanBench.Setup` — the `setup_benchmark` command

Usage:

```lean
setup_benchmark goodFib n => 2 ^ n
setup_benchmark mergeSort n => n * Nat.log2 n
```

The complexity expression on the right of `=>` has type `Nat → Nat`.
For `n log n` use `Nat.log2` (in core). For polynomial / exponential
use `^`.

The elaborator does only **static** checks at registration time:

- the function exists and has type `Nat → α`
- a `Hashable α` instance is searched (presence flags whether
  `runChecked` will produce a hash; absence is fine)
- the complexity term elaborates against `Nat → Nat`

It auto-generates two top-level defs and a single `register` call.
There is intentionally **no** elaboration-time subprocess spawning;
Codex review identified that as the worst design choice in the
original draft (circular and phase-dependent). Compiled-code sanity
checks live in `lean-bench verify` (a CLI subcommand) — see
[`LeanBench.Verify`](Verify.lean).

The `where { ... }` clause for overriding `BenchmarkConfig` defaults
is on the v0.2 roadmap; in v0.1 every benchmark uses default config.
-/

open Lean Elab Command Term Meta

namespace LeanBench

/--
`setup_benchmark` registers a benchmark.

The function being benchmarked must have type `Nat → α`. The
complexity expression after `=>` has type `Nat → Nat`; use `Nat.log2`
(from core) for `n log n`, `^` / `Nat.pow` for polynomial or
exponential.

If `α` has a `Hashable` instance, the auto-generated `runChecked`
emits a `UInt64` hash of the result. Otherwise it returns `none`
after forcing the result; conformance hashing is unavailable for
this benchmark.

An optional `with prep := <ident>` clause hoists per-param setup work
out of the inner timing loop. With `with prep := mkInput`, the
benchmarked function must have type `σ → α` where `σ` is the return
type of `mkInput : Nat → σ`. The macro generates a runner that calls
`mkInput param` once per batch and feeds the result into every inner
iteration, so only the work inside the benchmarked function gets
timed. Use this when the input the function operates on is expensive
to build (an array, a tree, a hashmap), but cheap to reuse.
-/
syntax (name := setupBenchmark) "setup_benchmark "
  ident ident " => " term (" with " &"prep" " := " ident)? : command

/-- Look up the type of a constant; return `(argTy, resTy)`. -/
def expectFunction (env : Environment) (declName : Name) :
    CommandElabM (Expr × Expr) := do
  let some ci := env.find? declName
    | throwError "setup_benchmark: function `{declName}` is not defined"
  let .forallE _ argTy resTy _ := ci.type
    | throwError "setup_benchmark: function `{declName}` is not a function (type: {ci.type})"
  if resTy.hasLooseBVars then
    throwError "setup_benchmark: function `{declName}`'s return type depends on its argument; only non-dependent functions are supported"
  return (argTy, resTy)

/-- Look up the type of a constant; assert it is `Nat → α` and return `α`. -/
def expectNatToAlpha (env : Environment) (declName : Name) :
    CommandElabM Expr := do
  let (argTy, resTy) ← expectFunction env declName
  unless argTy.isConstOf ``Nat do
    throwError "setup_benchmark: function `{declName}` must take a single `Nat` argument; got `{argTy}`"
  return resTy

/-- True iff `Hashable α` synthesizes. -/
def hasHashable (alphaExpr : Expr) : CommandElabM Bool := do
  liftTermElabM do
    let goal ← Meta.mkAppM ``Hashable #[alphaExpr]
    match ← (Meta.synthInstance? goal) with
    | some _ => return true
    | none => return false

/-- Resolve a `setup_benchmark` argument identifier against the current
namespace, falling back to the bare name. Returns the candidate even
on failure so the eventual error message names the user-visible
qualified form rather than the bare identifier. -/
def resolveDecl (env : Environment) (ns : Name) (id : Ident) : Name :=
  let bare := id.getId
  let candidate := ns ++ bare
  if env.contains candidate then candidate
  else if env.contains bare then bare
  else candidate

@[command_elab setupBenchmark]
def elabSetupBenchmark : CommandElab := fun stx => do
  match stx with
  | `(command| setup_benchmark $fnId:ident $argId:ident => $complexityTerm) =>
    elabCore fnId argId complexityTerm none
  | `(command| setup_benchmark $fnId:ident $argId:ident => $complexityTerm
        with prep := $prepId:ident) =>
    elabCore fnId argId complexityTerm (some prepId)
  | _ => throwUnsupportedSyntax
where
  elabCore (fnId argId : Ident) (complexityTerm : Term)
      (prepId? : Option Ident) : CommandElabM Unit := do
    let env ← getEnv
    let ns ← getCurrNamespace
    let fnName := resolveDecl env ns fnId
    -- Validate types, branching on whether a prep clause was given.
    -- `resTy` is the function's return type α (used for Hashable lookup).
    let resTy ← match prepId? with
      | none =>
        -- No prep: fn must be `Nat → α`.
        expectNatToAlpha env fnName
      | some prepStx =>
        -- With prep: `prep : Nat → σ` and `fn : σ → α`. Check the
        -- types line up.
        let prepName := resolveDecl env ns prepStx
        let σ ← expectNatToAlpha env prepName
        let (fnArgTy, fnResTy) ← expectFunction env fnName
        let typesMatch ← liftTermElabM <| Meta.isDefEq fnArgTy σ
        unless typesMatch do
          throwError "setup_benchmark: function `{fnName}` takes argument of type `{fnArgTy}`, but prep `{prepName}` returns `{σ}`"
        pure fnResTy
    let isHashable ← hasHashable resTy
    -- Names of the auto-generated helpers.
    let cName := fnName.str "_leanBench_complexity"
    let rName := fnName.str "_leanBench_runChecked"
    let cIdent := mkIdent cName
    let rIdent := mkIdent rName
    -- (1) auto-def the complexity function.
    elabCommand <| ← `(command|
      def $cIdent : Nat → Nat := fun $argId => $complexityTerm)
    -- (2) auto-def the specialised loop runner. The loop body lives
    -- in this generated def so the Lean compiler can inline the
    -- function under test directly into it (no closure indirection,
    -- no per-iteration Hashable / Option wrap on the hot path).
    --
    -- The runner is two-stage: it takes `param`, runs prep (if any),
    -- forces the prep value via `blackBox (Hashable.hash s)` so its
    -- cost stays out of the timed loop, and returns a closure
    -- `count → IO (loopNanos × hash)`. `IO.monoNanosNow` brackets
    -- only the inner `for` loop. The closure captures the prep
    -- result, so prep runs once per child-process spawn, not once
    -- per autotuner probe.
    --
    -- For the no-prep path, the input (`param : Nat`) is already
    -- strict and there is nothing to force; the runner is just
    -- `pure (fun count => …)`.
    let mkRunner : CommandElabM (TSyntax `command) := match prepId? with
      | none =>
        if isHashable then
          `(command|
            @[inline] def $rIdent (param : Nat) :
                IO (Nat → IO (Nat × Option UInt64)) := pure fun count => do
              if count = 0 then return (0, none)
              let mut last : UInt64 := 0
              let t₀ ← IO.monoNanosNow
              for _ in [0:count] do
                let r := $fnId param
                last := Hashable.hash r
                LeanBench.blackBox last
              let t₁ ← IO.monoNanosNow
              return (t₁ - t₀, some last))
        else
          `(command|
            @[inline] def $rIdent (param : Nat) :
                IO (Nat → IO (Nat × Option UInt64)) := pure fun count => do
              if count = 0 then return (0, none)
              let t₀ ← IO.monoNanosNow
              for _ in [0:count] do
                let r := $fnId param
                LeanBench.blackBox (Hashable.hash (sizeOf r))
              let t₁ ← IO.monoNanosNow
              return (t₁ - t₀, none))
      | some prepId =>
        if isHashable then
          `(command|
            @[inline] def $rIdent (param : Nat) :
                IO (Nat → IO (Nat × Option UInt64)) := do
              let s := $prepId param
              LeanBench.blackBox (Hashable.hash s)
              return fun count => do
                if count = 0 then return (0, none)
                let mut last : UInt64 := 0
                let t₀ ← IO.monoNanosNow
                for _ in [0:count] do
                  let r := $fnId s
                  last := Hashable.hash r
                  LeanBench.blackBox last
                let t₁ ← IO.monoNanosNow
                return (t₁ - t₀, some last))
        else
          `(command|
            @[inline] def $rIdent (param : Nat) :
                IO (Nat → IO (Nat × Option UInt64)) := do
              let s := $prepId param
              LeanBench.blackBox (Hashable.hash s)
              return fun count => do
                if count = 0 then return (0, none)
                let t₀ ← IO.monoNanosNow
                for _ in [0:count] do
                  let r := $fnId s
                  LeanBench.blackBox (Hashable.hash (sizeOf r))
                let t₁ ← IO.monoNanosNow
                return (t₁ - t₀, none))
    elabCommand (← mkRunner)
    -- (3) emit an `initialize` block that puts the runtime closure
    --     into the IO.Ref registry. We `quote` the `Name`s so the
    --     runtime registry key is a properly hierarchical `Name` —
    --     `Name.mkSimple "a.b.c"` would instead produce an atomic name
    --     whose `toString` wraps it in guillemets.
    -- `reprint` preserves trailing whitespace from the source, which
    -- would turn the `list` output into a spaced-out blank-line mess.
    let formulaStr :=
      (complexityTerm.raw.reprint.getD (toString complexityTerm.raw)).trimAscii.toString
    let formulaLit := Syntax.mkStrLit formulaStr
    let fnNameSyn : Term := quote fnName
    let cNameSyn  : Term := quote cName
    let rNameSyn  : Term := quote rName
    let isHashableSyn := mkIdent (if isHashable then ``true else ``false)
    elabCommand <| ← `(command|
      initialize do
        LeanBench.register
          { name := $fnNameSyn
            complexityName := $cNameSyn
            complexityFormula := $formulaLit
            runCheckedName := $rNameSyn
            hashable := $isHashableSyn
            config := {} }
          $rIdent
          $cIdent)
    -- (4) update the persistent env extension at elaboration time.
    let spec : BenchmarkSpec :=
      { name := fnName
        complexityName := cName
        complexityFormula := formulaStr
        runCheckedName := rName
        hashable := isHashable
        config := {} }
    modifyEnv (addSpec · spec)

end LeanBench
