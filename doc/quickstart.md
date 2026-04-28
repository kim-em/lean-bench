# Quickstart

A complete benchmark project in three files.

## 1. `lakefile.toml`

```toml
name = "myproject"
defaultTargets = ["MyProject"]

[[require]]
name = "lean-bench"
git = "https://github.com/kim-em/lean-bench.git"
rev = "main"

[[lean_lib]]
name = "MyProject"

[[lean_exe]]
name = "bench"
root = "MyProject.Bench"
```

## 2. `MyProject.lean`

```lean
def myFib : Nat → Nat
  | 0 => 0 | 1 => 1 | n + 2 => myFib n + myFib (n + 1)
```

## 3. `MyProject/Bench.lean`

```lean
import LeanBench
import MyProject

setup_benchmark myFib n => 2 ^ n

def main (args : List String) : IO UInt32 :=
  LeanBench.Cli.dispatch args
```

## Build and run

```bash
$ lake build bench
$ lake exe bench list
registered benchmarks:
  myFib    expected complexity: n + 1
$ lake exe bench run myFib
... result table ...
$ lake exe bench compare myFib someOtherFib
... comparison report ...
$ lake exe bench verify
verifying 1 benchmark(s)...
  [ok ] «myFib»
all 1 benchmark(s) passed
```

`verify` spawns the child path against `f 0` and `f 1` for each
registered benchmark (or just the named ones). Use it as a fast
smoke test that a registration is wired up correctly: child dispatch
works, the row is parseable, and (when the return type is `Hashable`)
the hashing path produces a hash. Each check is bounded by the
benchmark's `maxSecondsPerCall`. Exit code is non-zero if any check
fails.

## Optional: per-param setup with `with prep := …`

If your benchmarked function operates on a structure that's expensive
to build (a sorted array, a tree, a hashmap), declaring it as
`Nat → α` will fold the build cost into every timed iteration and
ruin the verdict — a `log n` search behind a `O(n)` setup looks
linear in the report. Use the optional `with prep := <ident>` clause
to hoist the setup out of the timing loop:

```lean
def mkArr (n : Nat) : Array Nat := (Array.range n).map (· * 7)

def prepInput (n : Nat) : Array Nat × Nat :=
  (mkArr n, (n.max 1 - 1) * 7 / 2)

partial def bsearch (a : Array Nat) (target : Nat) : Option Nat := ...

def runBinary (input : Array Nat × Nat) : Nat :=
  (bsearch input.1 input.2).getD 0

setup_benchmark runBinary n => Nat.log2 (n + 1)
  with prep := prepInput
```

Type contract: `prep : Nat → σ` and the benchmarked function has type
`σ → α`. The prep's return type `σ` must be `Hashable` (in the example
above, `Array Nat × Nat` derives it automatically); the macro hashes
the prep result before timing starts so its construction cost is
fully paid up front. Prep then runs once per child-process spawn —
not once per autotuner probe — and the inner loop calls the function
with the prepared value.

## Complexity expressions

The complexity expression on the right of `=>` has type `Nat → Nat`.
Pick whichever expression best models the algorithm:

| Asymptotic | Lean expression          |
|------------|--------------------------|
| O(1)       | `1`                      |
| O(n)       | `n`                      |
| O(n log n) | `n * Nat.log2 (n + 1)`   |
| O(n²)      | `n * n`                  |
| O(n³)      | `n * n * n`              |
| O(2ⁿ)      | `2 ^ n`                  |

`Nat.log2` is in core Lean. The `+ 1` in the `n log n` row guards
against `log2 0 = 0` producing a zero denominator in the ratio.

## Tags and filtering

As your benchmark suite grows, organizing benchmarks into groups makes
it easier to run subsets, compare families, and keep `list` output
readable.

### Tagging benchmarks

Add `tags` in the `where { … }` block:

```lean
setup_benchmark runInsertion n => n * n where {
  tags := #["sort", "quadratic"]
  maxSecondsPerCall := 0.5
}
setup_benchmark runMergeSort n => n * Nat.log2 (n + 1) where {
  tags := #["sort"]
}
setup_benchmark goodFib n => n where {
  tags := #["fib"]
}
```

Tags are arbitrary strings. A benchmark can have zero or more tags.
Benchmarks without tags work exactly as before — the feature is fully
opt-in.

Fixed benchmarks support tags too:

```lean
setup_fixed_benchmark heavyComputation where {
  tags := #["regression"]
  repeats := 10
}
```

### Filtering by tag or name

All parent-side subcommands (`list`, `run`, `compare`, `verify`)
accept `--tag` and `--filter` flags:

| Flag | Semantics |
|------|-----------|
| `--tag sort` | Benchmarks with the `sort` tag |
| `--tag sort,fib` | Benchmarks with `sort` OR `fib` (OR logic) |
| `--filter Sort` | Benchmarks whose name contains `Sort` |
| `--tag sort --filter Insert` | Both conditions must match (AND) |

Examples:

```bash
# List only sorting benchmarks
$ lake exe bench list --tag sort

# Run all benchmarks tagged "fib"
$ lake exe bench run --tag fib

# Compare all sorting benchmarks
$ lake exe bench compare --tag sort

# Verify benchmarks whose name contains "Sort"
$ lake exe bench verify --filter Sort
```

When no `--tag` or `--filter` is given, all benchmarks are included
(existing behavior). Name filtering matches anywhere in the fully
qualified dotted name, so `--filter Sort` matches
`MyProject.Sort.runMergeSort`.

### Using namespaces as organization

Lean namespaces naturally provide hierarchical grouping. Combined with
`--filter`, this gives you namespace-based filtering for free:

```lean
namespace Sort
  setup_benchmark runInsertion n => n * n
  setup_benchmark runMergeSort n => n * Nat.log2 (n + 1)
end Sort

namespace Fib
  setup_benchmark goodFib n => n
  setup_benchmark badFib  n => 144 ^ n / 89 ^ n
end Fib
```

```bash
$ lake exe bench list --filter Sort
$ lake exe bench compare --filter Sort
```

Tags and namespaces are complementary: namespaces give you
hierarchical grouping, tags give you cross-cutting categories
(e.g. `"fast"`, `"regression"`, `"nightly"`).

## Configuring a benchmark

`BenchmarkConfig` carries the knobs the harness reads at run time:
target inner-batch wall time, the per-batch wallclock cap, the
doubling-ladder bounds, the warmup-trim count, and the
slope-tolerance threshold for the verdict. Defaults are sensible
for typical workloads.

### Per-benchmark overrides at declaration time

Add a `where { … }` block to `setup_benchmark`. Anything you don't
mention keeps its default:

```lean
setup_benchmark slowQuadratic n => n * n where {
  maxSecondsPerCall := 0.5
  paramCeiling := 16384
}
```

### Per-run overrides at the command line

Both `run` and `compare` accept the same set of flags. They layer on
top of whatever was declared, so you can ship a tight default and
loosen or tighten it interactively:

```bash
$ lake exe bench run myFib --max-seconds-per-call 0.25 --param-ceiling 1024
$ lake exe bench compare goodFib badFib --warmup-fraction 0.3 --slope-tolerance 0.1
```

Available flags:

| Flag                     | Type           | `BenchmarkConfig` field   |
|--------------------------|----------------|---------------------------|
| `--max-seconds-per-call` | Float          | `maxSecondsPerCall`       |
| `--target-inner-nanos`   | Nat            | `targetInnerNanos`        |
| `--param-floor`          | Nat            | `paramFloor`              |
| `--param-ceiling`        | Nat            | `paramCeiling`            |
| `--warmup-fraction`      | Float          | `verdictWarmupFraction`   |
| `--slope-tolerance`      | Float          | `slopeTolerance`          |
| `--param-schedule`       | ParamSchedule  | `paramSchedule`           |
| `--cache-mode`           | CacheMode      | `cacheMode`               |
| `--outer-trials`         | Nat            | `outerTrials`             |

`--param-schedule` accepts `auto` (default — pick from declared
complexity), `doubling`, or `linear`. Use `--param-schedule linear`
to force the linear ladder on a benchmark whose declared model
fooled the auto-detect heuristic, or `--param-schedule doubling` to
force a wider log-x sweep. Pin a non-default sample count for
`linear` at declaration time via
`where { paramSchedule := .linear 32 }`. There is no CLI flag for
the sample count: `--param-schedule linear` on a benchmark already
declared with `.linear 32` keeps the declared `32`, since the flag
only carries the schedule kind. Switching from `.auto` / `.doubling`
to `.linear` via the CLI uses the macro-default 16 samples.

`--cache-mode warm|cold` selects what is being measured. The default
`warm` is the v0.1 design: the child auto-tunes an inner-repeat count
and runs the function many times in a single spawn, so caches and
branch predictors are primed across the repeats. `cold` respawns the
child for every ladder rung and runs the function exactly once per
spawn, so cache state is not preserved across measurements. Neither
mode is universally better — they measure different things; see
[advanced.md#cache-modes](advanced.md#cache-modes) for when to use
each. Pin a benchmark in cold mode at declaration time via
`where { cacheMode := .cold }`.

`--outer-trials N` runs `N` independent measurements per ladder rung
and reports per-param median / min / max / spread instead of a single
sample. Default `1` matches the v0.1 behaviour. Bumping to `3` or `5`
trades wall time for stability — useful on noisy CI hosts or when a
verdict is borderline. The raw per-trial points stay in the result so
nothing is lost. See [advanced.md#outer-trials](advanced.md#outer-trials)
for how to read the summary block and what its limits are. Pin at
declaration time via `where { outerTrials := 3 }`.

For workloads that don't fit any built-in ladder — corpus inputs
where the natural `n` values are non-uniform, or a single-rung
"this is the size that matters" run — declare a custom ladder:

```lean
setup_benchmark cornerCase n => n where {
  paramSchedule := .custom #[1, 100, 10_000, 1_000_000]
}
```

`.custom` skips the doubling probe and the linear-bracket sweep
entirely; the harness walks the declared params in order and stops
when one hits the wallclock cap. There is no CLI flag for `.custom`
because the param list can't be cleanly expressed as one — declare
it at registration time. An empty `.custom #[]` is rejected by
config validation.

`--warmup-fraction F` drops the leading `F` × ratios.size data points
from the verdict reduction (the cold regime where per-call overhead
dominates the declared complexity); the raw ratios array still
contains every measured point. `--slope-tolerance T` is the verdict
threshold on the log-log slope β of `C` vs `param`:
`consistentWithDeclaredComplexity` requires `|β| ≤ T` over the
trimmed tail.

The `Float` flags accept JSON-style numbers (`0.5`, `1e-3`, `3`).
Lean shells in the unusual cases too: `+1`, `.5`, and `1.` are
**not** accepted; write `1`, `0.5`, and `1.0`. Negative values are
syntactically valid but rejected by the harness's config validation
(non-positive caps and ratios make no sense).

## Reading the output

Per-data-point ratio `C = perCallNanos / complexity(param)`. If your
declared complexity matches the implementation, `C` is approximately
constant. The verdict fits the log-log slope β of `C` vs `param` over
the trimmed tail: "consistent with declared complexity" when
`|β| ≤ 0.15`, otherwise "inconclusive". β is printed on the verdict
line; its sign tells you the direction of any mismatch (positive →
actually slower than declared, negative → actually faster).

The harness prints its own per-spawn floor as part of the report. Any
data point with `total_nanos` smaller than `signalFloorMultiplier ×
spawn_floor` (default 10×) is flagged with `[<floor]` in the table
and excluded from the verdict — its per-call time is dominated by
subprocess-spawn cost, not the function under test. Tune the
threshold via `where { signalFloorMultiplier := 5.0 }` (looser) or
`signalFloorMultiplier := 1.0` to disable the filter entirely.

When the harness can't trust the data — every row was below the
floor, every row hit the cap, the ladder was truncated by the cap,
or fewer than three rows survived the verdict reduction — it
appends one or more `‼` advisory lines after the verdict, each
naming a concrete knob to turn next (`--max-seconds-per-call`,
`--param-ceiling`, `paramSchedule := .custom #[...]`,
`setup_fixed_benchmark`, …). The advisory text is the actionable
half of issue #15; the underlying classifications live on
`BenchmarkResult.advisories` for downstream tooling.

## What `compare` shows on divergence

`compare A B [C …]` runs each named benchmark, intersects their
common params, and hashes every result. When the hashes disagree
the report highlights the earliest common param where any pair
disagreed, lays the per-function hash table side-by-side, and tags
each dissenting implementation with the baseline it disagrees with
(the first listed implementation). Later disagreements appear on a
compact `also diverged at:` line so you can tell a one-off
mismatch from a fundamental disagreement.

```
agreement: DIVERGED on 2 params — earliest divergence at param=4:
    MyApp.fastSort  hash=0xabc    (baseline)
    MyApp.slowSort  hash=0xdef    differs from MyApp.fastSort
  also diverged at: 8
```

Today only hashes are recorded — no string preview of the result is
captured. A 64-bit hash tells you *that* two implementations
disagree but not *what* the outputs were. The recommended workflow
when you see a divergence:

1. Note the earliest diverging param `n` and the implementations
   involved from the report.
2. Run each implementation directly at that input and inspect the
   results yourself, e.g. via `#eval MyApp.fastSort (gen 4)` and
   `#eval MyApp.slowSort (gen 4)` in a Lean file alongside the
   benchmarks. Pick a small `n` so the output is human-readable.
3. If the implementations take a prepared input (`with prep := …`),
   construct that input the same way the prep does and feed it to
   both directly.

If at least one of the compared functions has a non-`Hashable`
return type, `compare` cannot check agreement at all and reports
`agreement: cannot check — no Hashable instance for: <name(s)>`.
The fix is to add `deriving Hashable` (or write a manual instance)
on the return type so the harness can record hashes. Future
versions may add an optional preview field on top of the hashes;
the wire format already tolerates additive optional fields without
a schema bump (see [`schema.md`](schema.md)).

## Fixed-problem benchmarks

Sometimes you don't have a parameter to walk: there's a single hard
input and you want to record an absolute wall-clock number on it,
either to compare against an external reference (FLINT, fpLLL, …) or
to catch absolute-performance regressions over time. The
`setup_fixed_benchmark` macro registers a benchmark of this shape.

The function name *is* the benchmark name and must resolve to either
`α` (a pure value) or `IO α` (an effectful computation, useful when
the workload reads input from disk or shells out to an external
tool):

```lean
def factorXOverFTwo : Polynomial Bool := -- ... single hard problem
setup_fixed_benchmark factorXOverFTwo

def runFplll : IO LLLOutput := -- ... shell out
setup_fixed_benchmark runFplll where {
  repeats := 10
  maxSecondsPerCall := 30.0
}
```

There is no `Nat` parameter, no complexity expression, and no verdict.
The runner does one warmup call, then `repeats` measured calls, and
emits one JSONL row per repeat with `kind:"fixed"`. The report shows
median / min / max plus a hash-agreement check across repeats — a
divergence flags a non-deterministic registration.

The same `run` and `compare` subcommands work; they dispatch by
registration kind. The `list` subcommand annotates fixed entries with
`[fixed]`. `verify` covers fixed benchmarks too — a single
sanity-check spawn per registration.

`FixedBenchmarkConfig` knobs:

| Field | Default | Purpose |
|-------|---------|---------|
| `repeats` | `5` | Number of measured invocations after the warmup |
| `maxSecondsPerCall` | `60.0` | Hard wallclock cap per invocation (seconds) |
| `killGraceMs` | `100` | Grace ms between SIGTERM and SIGKILL |
| `warmup` | `true` | Whether to perform a single discarded warmup call |

The CLI flag `--repeats N` overrides the declared `repeats` per run;
`--max-seconds-per-call` is shared with parametric. Parametric-only
flags (`--param-ceiling`, `--slope-tolerance`, …) are silently ignored
when dispatching a fixed benchmark.

`compare A B` requires that all listed names be the same kind
(parametric or fixed). The fixed comparison report prints a
relative-timing line — each function's ratio against the first —
which is the v0.2 hex use case for "this took ~2× FLINT."

## Caveats

- The wallclock cap is enforced via a pure-Lean kill path
  (`IO.Process.spawn` + `IO.Process.Child.kill`), so the harness
  works on every platform Lean's process API supports — Linux,
  macOS, and Windows — with no external `timeout(1)` dependency.
- The verdict is heuristic; the raw `ratios` array is the source of
  truth. See [PLAN.md](../PLAN.md) for v0.2 plans (auto-fit,
  baseline-diff, …).
- Lean's `Nat` is arbitrary-precision. If your function returns big
  integers, the actual complexity includes the cost of arithmetic on
  results — likely worse than the "obvious" textbook complexity. See
  [pitfalls.md](pitfalls.md) for the full set of Lean-specific
  benchmarking traps (bignum `Nat`, forcing evaluation, hashing cost,
  sharing-induced regressions, compiled vs. `#eval`, warm vs. cold).
