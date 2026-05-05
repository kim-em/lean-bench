import Lean
import LeanBench.Core
import LeanBench.Env

/-!
# `LeanBench.Setup` — the `setup_benchmark` command

Usage:

```lean
setup_benchmark goodFib n => 2 ^ n
setup_benchmark mergeSort n => n * Nat.log2 n
setup_benchmark slowThing n => 2 ^ n where {
  maxSecondsPerCall := 5.0
  paramCeiling := 1024
}
```

The complexity expression on the right of `=>` has type `Nat → Nat`.
For `n log n` use `Nat.log2` (in core). For polynomial / exponential
use `^`.

The elaborator does only **static** checks at registration time:

- the function exists and has type `Nat → α`
- a `Hashable α` instance is searched (presence flags whether
  `runChecked` will produce a hash; absence is fine)
- the complexity term elaborates against `Nat → Nat`
- the optional `where { … }` clause elaborates against `BenchmarkConfig`

It auto-generates the complexity function, the specialised loop runner,
the per-benchmark config def, and a single `register` call.
There is intentionally **no** elaboration-time subprocess spawning:
making the elaborator depend on the compiled binary is circular and
phase-dependent. Compiled-code sanity checks live in `lean-bench
verify` (a CLI subcommand) — see [`LeanBench.Verify`](Verify.lean).

## `where { ... }` overrides

The optional `where { field := expr, ... }` clause overrides individual
`BenchmarkConfig` fields. Anything you don't mention keeps its default.
Field names match `BenchmarkConfig`. Run-time CLI overrides
(`--max-seconds-per-call`, `--param-ceiling`, …) layer on top of these
declared values, so a benchmark can ship a sensible default and still
be tightened from the command line.
-/

open Lean Elab Command Term Meta

namespace LeanBench

/-- The trailing `where TERM` clause on `setup_benchmark`. `TERM` is
elaborated against `LeanBench.BenchmarkConfig`, so the natural shape
is a structure literal: `where { maxSecondsPerCall := 5.0 }`. -/
syntax setupBenchmarkWhere := " where " term

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

An optional trailing `where { … }` clause overrides individual
`BenchmarkConfig` fields. The two clauses can appear together; `with
prep := …` comes first.
-/
syntax (name := setupBenchmark) "setup_benchmark "
  ident ident " => " term (" with " &"prep" " := " ident)?
                          (setupBenchmarkWhere)? : command

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
    elabCore fnId argId complexityTerm none none
  | `(command| setup_benchmark $fnId:ident $argId:ident => $complexityTerm
        with prep := $prepId:ident) =>
    elabCore fnId argId complexityTerm (some prepId) none
  | `(command| setup_benchmark $fnId:ident $argId:ident => $complexityTerm
        $w:setupBenchmarkWhere) =>
    elabCore fnId argId complexityTerm none (some w)
  | `(command| setup_benchmark $fnId:ident $argId:ident => $complexityTerm
        with prep := $prepId:ident
        $w:setupBenchmarkWhere) =>
    elabCore fnId argId complexityTerm (some prepId) (some w)
  | _ => throwUnsupportedSyntax
where
  elabCore (fnId argId : Ident) (complexityTerm : Term)
      (prepId? : Option Ident)
      (whereClause? : Option (TSyntax `LeanBench.setupBenchmarkWhere)) :
      CommandElabM Unit := do
    let env ← getEnv
    let ns ← getCurrNamespace
    let fnName := resolveDecl env ns fnId
    -- `resTy` is α (used below for Hashable lookup). With prep:
    -- require `prep : Nat → σ` and `fn : σ → α`.
    let resTy ← match prepId? with
      | none =>
        expectNatToAlpha env fnName
      | some prepStx =>
        let prepName := resolveDecl env ns prepStx
        let σ ← expectNatToAlpha env prepName
        let (fnArgTy, fnResTy) ← expectFunction env fnName
        let typesMatch ← liftTermElabM <| Meta.isDefEq fnArgTy σ
        unless typesMatch do
          throwError "setup_benchmark: function `{fnName}` takes argument of type `{fnArgTy}`, but prep `{prepName}` returns `{σ}`"
        pure fnResTy
    let isHashable ← hasHashable resTy
    let configTerm? : Option Term ← match whereClause? with
      | none => pure none
      | some w => match w with
        | `(setupBenchmarkWhere| where $t:term) => pure (some t)
        | _ => throwErrorAt w "setup_benchmark: malformed `where` clause"
    let cName := fnName.str "_leanBench_complexity"
    let rName := fnName.str "_leanBench_runChecked"
    let cfgDeclName := fnName.str "_leanBench_config"
    let cIdent := mkIdent cName
    let rIdent := mkIdent rName
    let cfgIdent := mkIdent cfgDeclName
    elabCommand <| ← `(command|
      def $cIdent : Nat → Nat := fun $argId => $complexityTerm)
    -- The loop body is in a generated def so the compiler can inline
    -- the function under test (no closure indirection, no per-iteration
    -- Hashable / Option wrap on the hot path).
    --
    -- The runner is two-stage: take `param`, run prep (if any), force
    -- the prep value via `blackBox` so its cost stays out of the timed
    -- loop, and return a closure `count → IO (loopNanos × hash)`. The
    -- closure captures the prep result, so prep runs once per child-
    -- process spawn rather than once per autotuner probe.
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
    match configTerm? with
    | none =>
      elabCommand <| ← `(command|
        def $cfgIdent : LeanBench.BenchmarkConfig := {})
    | some t =>
      elabCommand <| ← `(command|
        def $cfgIdent : LeanBench.BenchmarkConfig := $t)
    -- `quote` the `Name` so the runtime registry key stays
    -- hierarchical; `Name.mkSimple "a.b.c"` would produce an atomic
    -- name whose `toString` wraps it in guillemets.
    -- `reprint` preserves trailing whitespace from source, which
    -- would turn `list` output into spaced-out blank lines.
    let formulaStr :=
      (complexityTerm.raw.reprint.getD (toString complexityTerm.raw)).trimAscii.toString
    let formulaLit := Syntax.mkStrLit formulaStr
    let fnNameSyn  : Term := quote fnName
    let isHashableSyn := mkIdent (if isHashable then ``true else ``false)
    elabCommand <| ← `(command|
      initialize do
        LeanBench.register
          { name := $fnNameSyn
            complexityFormula := $formulaLit
            hashable := $isHashableSyn
            config := $cfgIdent }
          $rIdent
          $cIdent)

/-! ## `setup_fixed_benchmark` — fixed-problem registration

```lean
setup_fixed_benchmark Hex.Foo.factorXOverFTwo
setup_fixed_benchmark MyBench.lllReduce30 where {
  repeats := 10
  maxSecondsPerCall := 30.0
}
```

The registered name must resolve to a value of type `α` (pure) or
`IO α` (effectful). There is no parameter and no complexity model —
the benchmark exists to record an absolute wall-clock time on a
canonical input. The harness performs one warmup call followed by
`config.repeats` measured calls and reports median / min / max plus
a hash-agreement check across repeats.

If `α` has a `Hashable` instance, every measured call's result is
hashed; cross-repeat hash agreement guards against non-determinism.
Without `Hashable`, the comparison falls back to timing only. -/

/-- Inspect a registered name's type. Returns `(elementTy, isIO)`
where `elementTy` is `α` for both `α` and `IO α` cases. The IO check
is semantic, not syntactic: we try to unify the declared type against
`IO ?α`, which means `EIO IO.Error α`, `BaseIO α` (after reducible
unfolding), and any reducible alias of `IO` are all accepted. A bare
syntactic `isAppOfArity ``IO 1` would miss those and silently
benchmark the IO action as a pure value (a serious correctness bug),
so we use full `isDefEq`.

Rejects anything with an unbound function arrow — fixed benchmarks
are values, not functions. -/
def expectFixedValue (env : Environment) (declName : Name) :
    CommandElabM (Expr × Bool) := do
  let some ci := env.find? declName
    | throwError "setup_fixed_benchmark: name `{declName}` is not defined"
  let ty := ci.type
  if ty.isForall then
    throwError "setup_fixed_benchmark: name `{declName}` must be a value or `IO α`; got function type `{ty}`"
  liftTermElabM do
    let α ← Meta.mkFreshExprMVar (mkSort Level.one)
    let ioα ← Meta.mkAppM ``IO #[α]
    if (← Meta.isDefEq ty ioα) then
      let elemTy ← instantiateMVars α
      return (elemTy, true)
    return (ty, false)

syntax (name := setupFixedBenchmark) "setup_fixed_benchmark "
  ident (setupBenchmarkWhere)? : command

@[command_elab setupFixedBenchmark]
def elabSetupFixedBenchmark : CommandElab := fun stx => do
  match stx with
  | `(command| setup_fixed_benchmark $fnId:ident) =>
    elabCore fnId none
  | `(command| setup_fixed_benchmark $fnId:ident $w:setupBenchmarkWhere) =>
    elabCore fnId (some w)
  | _ => throwUnsupportedSyntax
where
  elabCore (fnId : Ident)
      (whereClause? : Option (TSyntax `LeanBench.setupBenchmarkWhere)) :
      CommandElabM Unit := do
    let env ← getEnv
    let ns ← getCurrNamespace
    let fnName := resolveDecl env ns fnId
    let (resTy, isIO) ← expectFixedValue env fnName
    let isHashable ← hasHashable resTy
    let configTerm? : Option Term ← match whereClause? with
      | none => pure none
      | some w => match w with
        | `(setupBenchmarkWhere| where $t:term) => pure (some t)
        | _ => throwErrorAt w "setup_fixed_benchmark: malformed `where` clause"
    let rName := fnName.str "_leanBench_fixedRunner"
    let cfgDeclName := fnName.str "_leanBench_fixedConfig"
    let rIdent := mkIdent rName
    let cfgIdent := mkIdent cfgDeclName
    -- Four shapes from `(isIO, isHashable)`. All bracket the timer
    -- around one invocation; the result is forced via `blackBox` to
    -- defeat dead-code elimination, like the parametric path.
    let mkRunner : CommandElabM (TSyntax `command) :=
      match isIO, isHashable with
      | true, true =>
        `(command|
          @[inline] def $rIdent : IO (Nat × Option UInt64) := do
            let t₀ ← IO.monoNanosNow
            let r ← ($fnId : IO _)
            let h := Hashable.hash r
            LeanBench.blackBox h
            let t₁ ← IO.monoNanosNow
            return (t₁ - t₀, some h))
      | true, false =>
        `(command|
          @[inline] def $rIdent : IO (Nat × Option UInt64) := do
            let t₀ ← IO.monoNanosNow
            let r ← ($fnId : IO _)
            LeanBench.blackBox (Hashable.hash (sizeOf r))
            let t₁ ← IO.monoNanosNow
            return (t₁ - t₀, none))
      | false, true =>
        `(command|
          @[inline] def $rIdent : IO (Nat × Option UInt64) := do
            let t₀ ← IO.monoNanosNow
            let r := $fnId
            let h := Hashable.hash r
            LeanBench.blackBox h
            let t₁ ← IO.monoNanosNow
            return (t₁ - t₀, some h))
      | false, false =>
        `(command|
          @[inline] def $rIdent : IO (Nat × Option UInt64) := do
            let t₀ ← IO.monoNanosNow
            let r := $fnId
            LeanBench.blackBox (Hashable.hash (sizeOf r))
            let t₁ ← IO.monoNanosNow
            return (t₁ - t₀, none))
    elabCommand (← mkRunner)
    match configTerm? with
    | none =>
      elabCommand <| ← `(command|
        def $cfgIdent : LeanBench.FixedBenchmarkConfig := {})
    | some t =>
      elabCommand <| ← `(command|
        def $cfgIdent : LeanBench.FixedBenchmarkConfig := $t)
    let fnNameSyn  : Term := quote fnName
    let isHashableSyn := mkIdent (if isHashable then ``true else ``false)
    elabCommand <| ← `(command|
      initialize do
        LeanBench.registerFixed
          { name := $fnNameSyn
            hashable := $isHashableSyn
            config := $cfgIdent }
          $rIdent)

end LeanBench
