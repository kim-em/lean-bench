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
checks live in `lean-bench verify` (a CLI subcommand) instead.

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
-/
syntax (name := setupBenchmark) "setup_benchmark "
  ident ident " => " term : command

/-- Look up the type of a constant; assert it is `Nat → α` and return `α`. -/
def expectNatToAlpha (env : Environment) (declName : Name) :
    CommandElabM Expr := do
  let some ci := env.find? declName
    | throwError "setup_benchmark: function `{declName}` is not defined"
  let .forallE _ argTy resTy _ := ci.type
    | throwError "setup_benchmark: function `{declName}` is not a function (type: {ci.type})"
  unless argTy.isConstOf ``Nat do
    throwError "setup_benchmark: function `{declName}` must take a single `Nat` argument; got `{argTy}`"
  if resTy.hasLooseBVars then
    throwError "setup_benchmark: function `{declName}`'s return type depends on its argument; only non-dependent functions are supported"
  return resTy

/-- True iff `Hashable α` synthesizes. -/
def hasHashable (alphaExpr : Expr) : CommandElabM Bool := do
  liftTermElabM do
    let goal ← Meta.mkAppM ``Hashable #[alphaExpr]
    match ← (Meta.synthInstance? goal) with
    | some _ => return true
    | none => return false

@[command_elab setupBenchmark]
def elabSetupBenchmark : CommandElab := fun stx => do
  match stx with
  | `(command| setup_benchmark $fnId:ident $argId:ident => $complexityTerm) => do
    let env ← getEnv
    -- Resolve the identifier relative to the current namespace, so
    -- `setup_benchmark goodFib …` finds `LeanBench.Examples.Fib.goodFib`
    -- when the call lives inside that namespace. We try the
    -- current-namespace-prefixed name first, then fall back to the
    -- bare identifier.
    let bare := fnId.getId
    let ns ← getCurrNamespace
    let candidate := ns ++ bare
    let fnName :=
      if env.contains candidate then candidate
      else if env.contains bare then bare
      else candidate  -- let `expectNatToAlpha` produce the helpful error
    let resTy ← expectNatToAlpha env fnName
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
    if isHashable then
      elabCommand <| ← `(command|
        @[inline] def $rIdent (count param : Nat) : IO (Option UInt64) := do
          if count = 0 then return none
          let mut last : UInt64 := 0
          for _ in [0:count] do
            let r := $fnId param
            last := Hashable.hash r
            LeanBench.blackBox last
          return some last)
    else
      elabCommand <| ← `(command|
        @[inline] def $rIdent (count param : Nat) : IO (Option UInt64) := do
          if count = 0 then return none
          for _ in [0:count] do
            let r := $fnId param
            LeanBench.blackBox (Hashable.hash (sizeOf r))
          return none)
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
  | _ => throwUnsupportedSyntax

end LeanBench
