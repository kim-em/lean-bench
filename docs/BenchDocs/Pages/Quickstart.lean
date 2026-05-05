import VersoManual
import LeanBench
import BenchDocs.Block.Bench
import BenchDocs.Block.TempProj

open Verso.Genre Manual
open Verso.Genre.Manual.InlineLean
open BenchDocs.Block

#doc (Manual) "Quickstart" =>

A complete benchmark project in three files. The project below is
materialised in a temporary directory and built end-to-end at
doc-build time, so a downstream user following these steps will see
exactly what this page shows — any drift in the lean-bench API or
the lakefile.toml conventions fails CI here first.

::: tempProj (tag := "myproject-3files")

```tempProjFile (path := "lakefile.toml")
name = "myproject"
defaultTargets = ["bench"]

[[require]]
name = "lean-bench"
path = "{{LEAN_BENCH_PATH}}"

[[lean_lib]]
name = "MyProject"

[[lean_exe]]
name = "bench"
root = "MyProject.Bench"
```

```tempProjFile (path := "MyProject.lean")
def myFib : Nat → Nat
  | 0 => 0 | 1 => 1 | n + 2 => myFib n + myFib (n + 1)
```

```tempProjFile (path := "MyProject/Bench.lean")
import LeanBench
import MyProject

setup_benchmark myFib n => 2 ^ n

def main (args : List String) : IO UInt32 :=
  LeanBench.Cli.dispatch args
```

```tempProjRun (args := "bench list")
registered benchmarks:
  myFib    expected complexity: 2 ^ n
```

:::

In real downstream projects, replace the `path = "{{LEAN_BENCH_PATH}}"`
require with a `git`-based one:

```
[[require]]
name = "lean-bench"
git = "https://github.com/kim-em/lean-bench.git"
rev = "main"
```

The `{{LEAN_BENCH_PATH}}` token is substituted at doc-build time with
the absolute path to the lean-bench checkout under test, so the
example actually compiles against the in-tree lean-bench rather than
chasing a remote.

# Build and run

The same project's `bench` exe supports `list`, `verify`, `run`, and
`compare` subcommands.

```bench (prog := "../.lake/build/bin/fib_benchmark_example") (argv := "list")
registered benchmarks:
  LeanBench.Examples.Fib.goodFib    expected complexity: n
  LeanBench.Examples.Fib.badFib    expected complexity: 144 ^ n / 89 ^ n
```

`verify` is the fastest sanity-check: it spawns the child path against
`f 0` and `f 1` for each registered benchmark, exits non-zero if any
check fails. Each check is bounded by the benchmark's
`maxSecondsPerCall`.

```bench (prog := "../.lake/build/bin/fib_benchmark_example") (argv := "verify")
verifying 2 benchmark(s)...
  [ok ] LeanBench.Examples.Fib.goodFib
  [ok ] LeanBench.Examples.Fib.badFib
all 2 benchmark(s) passed
```

# Optional: per-param setup with `with prep := …`

If your benchmarked function operates on a structure that's expensive
to build (a sorted array, a tree, a hashmap), declaring it as
`Nat → α` will fold the build cost into every timed iteration and
ruin the verdict — a `log n` search behind a `O(n)` setup looks linear
in the report. Use the optional `with prep := <ident>` clause to hoist
the setup out of the timing loop:

```lean
def mkArr (n : Nat) : Array Nat := (Array.range n).map (· * 7)

def prepInput (n : Nat) : Array Nat × Nat :=
  (mkArr n, (n.max 1 - 1) * 7 / 2)

partial def bsearch (a : Array Nat) (target : Nat) : Option Nat := Id.run do
  let mut lo := 0
  let mut hi := a.size
  while lo < hi do
    let mid := (lo + hi) / 2
    if a[mid]! < target then lo := mid + 1
    else hi := mid
  if lo < a.size && a[lo]! == target then some lo else none

def runBinary (input : Array Nat × Nat) : Nat :=
  (bsearch input.1 input.2).getD 0
```

Type contract: `prep : Nat → σ` and the benchmarked function has type
`σ → α`. The prep's return type `σ` must be `Hashable` (`Array Nat × Nat`
derives it automatically); the macro hashes the prep result before timing
starts so its construction cost is fully paid up front. Prep then runs
once per child-process spawn — not once per autotuner probe — and the
inner loop calls the function with the prepared value.

# Complexity expressions

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

# Tags and filtering

As your benchmark suite grows, organising benchmarks into groups
makes it easier to run subsets, compare families, and keep `list`
output readable.

Add `tags` in the `where { … }` block:

```lean
def runInsertion (_ : Nat) : Nat := 0
def runMergeSort (_ : Nat) : Nat := 0
def goodFib (_ : Nat) : Nat := 0

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
Benchmarks without tags work exactly as before — the feature is
fully opt-in.

All parent-side subcommands (`list`, `run`, `compare`, `verify`)
accept `--tag` and `--filter` flags:

| Flag | Semantics |
|------|-----------|
| `--tag sort` | Benchmarks with the `sort` tag |
| `--tag sort,fib` | Benchmarks with `sort` OR `fib` (OR logic) |
| `--filter Sort` | Benchmarks whose name contains `Sort` |
| `--tag sort --filter Insert` | Both conditions must match (AND) |

When no `--tag` or `--filter` is given, all benchmarks are included.
Name filtering matches anywhere in the fully-qualified dotted name,
so `--filter Sort` matches `MyProject.Sort.runMergeSort`.

Lean namespaces naturally provide hierarchical grouping. Combined
with `--filter`, this gives you namespace-based filtering for free.
Tags and namespaces are complementary: namespaces give you
hierarchical grouping, tags give you cross-cutting categories
(e.g. `"fast"`, `"regression"`, `"nightly"`).

# CI-budget mode

For CI you usually want a benchmark suite that takes a predictable
amount of time, not whatever total a full sweep happens to land on
that day. `run` accepts a `--total-seconds N` flag that bounds the
whole suite to roughly `N` seconds of wallclock time. It is a
first-class feature: the harness schedules benchmarks against a
monotonic-clock deadline rather than relying on shell-script
`timeout` wrappers around the whole run.

Mechanics:

1. The orchestrator captures the start time and computes a deadline.
2. Before each benchmark, it checks the deadline. Benchmarks that
   would start past the deadline are recorded as `budget_skip`
   entries — they appear in the terminal output and the export
   document.
3. The deadline is also threaded into `runBenchmark`. A benchmark
   that's mid-ladder when the deadline trips finishes its current
   rung (each rung is independently capped by `maxSecondsPerCall`)
   and then stops; the result is tagged `budget_truncated`.
4. The run still produces a partial verdict for whatever rungs
   landed. Partial results aren't an error — they're the point of
   budget mode.

The bound on total wallclock time is approximately
`total_seconds + maxSecondsPerCall`: at most one rung can be in
flight when the deadline trips. There is no in-rung kill: a
benchmark with a 30s `maxSecondsPerCall` can blow past a 5s budget
by up to 30s. Tighten the per-call cap if you need a tighter bound.

A budgeted run prints a one-line summary at the bottom plus a
bullet per skipped benchmark:

```
budget: 60.000s; elapsed 58.910s; completed 7, skipped 2, truncated 1
  [budget skip] MyProject.Sort.runMergeSort    [parametric]
  [budget skip] MyProject.Sort.runInsertion    [parametric]
```

When `--export-file FILE` is also passed, the export document gains
a top-level `budget` object describing what happened. Each result
entry in `results[]` also carries `"budget_truncated": true|false`.

`run` and `compare` use distinct non-zero codes so CI can react
without parsing stdout:

| code | meaning |
|------|---------|
| `0`  | success — the run completed and (with `--baseline`) no regressions were flagged |
| `1`  | a regression vs. the baseline was flagged, or argument validation / dispatch errored |
| `2`  | the parametric run produced *zero verdict-eligible rows* |

Code `2` is the right signal to fail-loud on rather than swallow:
the verdict text would otherwise read `inconclusive (cMin=—,
cMax=—, β=—)` and look like a benign "couldn't decide", masking the
underlying calibration bug.

# Configuring a benchmark

`BenchmarkConfig` carries the knobs the harness reads at run time:
target inner-batch wall time, the per-batch wallclock cap, the
doubling-ladder bounds, the warmup-trim count, and the
slope-tolerance threshold for the verdict. Defaults are sensible
for typical workloads.

Add a `where { … }` block to `setup_benchmark`. Anything you don't
mention keeps its default:

```lean
def slowQuadratic (_ : Nat) : Nat := 0

setup_benchmark slowQuadratic n => n * n where {
  maxSecondsPerCall := 0.5
  paramCeiling := 16384
}
```

Both `run` and `compare` accept the same set of flags. They layer
on top of whatever was declared, so you can ship a tight default
and loosen or tighten it interactively. The `Float` flags accept
JSON-style numbers (`0.5`, `1e-3`, `3`); `+1`, `.5`, and `1.` are
*not* accepted — write `1`, `0.5`, and `1.0`.

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
| `--auto-fit`             | (boolean flag) | (heuristic — see below)   |

`--param-schedule` accepts `auto` (default), `doubling`, or `linear`.
`--cache-mode warm|cold` selects what is being measured. The default
`warm` is the v0.1 design: the child auto-tunes an inner-repeat count
and runs the function many times in a single spawn. `cold` respawns
the child for every ladder rung. `--outer-trials N` runs `N`
independent measurements per rung and reports per-param median /
min / max / spread instead of a single sample.

For workloads that don't fit any built-in ladder — corpus inputs
where the natural `n` values are non-uniform, or a single-rung
"this is the size that matters" run — declare a custom ladder:

```lean
def cornerCase (_ : Nat) : Nat := 0

setup_benchmark cornerCase n => n where {
  paramSchedule := .custom #[1, 100, 10_000, 1_000_000]
}
```

`.custom` skips the doubling probe and the linear-bracket sweep
entirely; the harness walks the declared params in order and stops
when one hits the wallclock cap. An empty `.custom #[]` is rejected
by config validation.

# Reading the output

Per-data-point ratio `C = perCallNanos / complexity(param)`. If your
declared complexity matches the implementation, `C` is approximately
constant. The verdict fits the log-log slope β of `C` vs `param`
over the trimmed tail: "consistent with declared complexity" when
`|β| ≤ 0.15`, otherwise "inconclusive". β is printed on the verdict
line; its sign tells you the direction of any mismatch (positive →
actually slower than declared, negative → actually faster).

The harness prints its own per-spawn floor as part of the report.
Any data point with `total_nanos` smaller than
`signalFloorMultiplier × spawn_floor` (default 10×) is flagged with
`[<floor]` in the table and excluded from the verdict — its per-call
time is dominated by subprocess-spawn cost, not the function under
test.

When the harness can't trust the data — every row was below the
floor, every row hit the cap, the ladder was truncated by the cap,
or fewer than three rows survived the verdict reduction — it
appends one or more `‼` advisory lines after the verdict, each
naming a concrete knob to turn next (`--max-seconds-per-call`,
`--param-ceiling`, `paramSchedule := .custom #[...]`,
`setup_fixed_benchmark`, …).

# Auto-fit

`setup_benchmark` requires you to declare a complexity model.
That's the right workflow when you know what you expect — the
verdict catches mismatches against the declared model. But it's
not the only useful workflow. Sometimes you have a function whose
complexity is genuinely unclear and you'd like the tool to suggest
one.

Pass `--auto-fit` on `run` (or `compare`) to see this. After the
standard verdict block, the report prints a ranked list of catalog
models by goodness-of-fit. The catalog is fixed:

| Catalog entry            | What it represents     |
|--------------------------|------------------------|
| `1`                      | constant time          |
| `n`                      | linear                 |
| `n * Nat.log2 (n + 1)`   | linearithmic           |
| `n^2`                    | quadratic              |
| `n^3`                    | cubic                  |
| `2^n`                    | exponential            |

The ranking score is `stdLogC` — the standard deviation of `log C`
across the verdict-eligible rungs, where `C = perCallNanos / model(param)`.
A perfect fit drives `C` to a constant (`stdLogC = 0`); the worse a
model is, the more `C` has to vary to explain the data, and the
higher its score. Every model is scored against the *same* rung
set, so the column is directly comparable across catalog entries.
The table is sorted by score so the runner-ups make it easy to see
how decisive the pick was.

The arrow tags the best fit only when the verdict is *decisive* —
≥ 3 usable rungs survived AND the runner-up's score is at least
1.5× the winner's. On a narrow ladder where adjacent catalog
entries can't be separated, no arrow is drawn and a `‼ inconclusive`
line names the reason.

This is a heuristic, not a proof. The catalog is fixed at six
entries, so anything not in the catalog gets the closest neighbour;
adjacent catalog entries (e.g. `n` vs `n * Nat.log2 (n + 1)`) are
hard to discriminate on short ladders. Read `stdLogC` to gauge how
good the closest match actually is; values above ~1.0 mean even the
winner doesn't fit well.

# What `compare` shows on divergence

`compare A B [C …]` runs each named benchmark, intersects their
common params, and hashes every result. When the hashes disagree
the report highlights the earliest common param where any pair
disagreed, lays the per-function hash table side-by-side, and tags
each dissenting implementation with the baseline it disagrees with
(the first listed implementation). Later disagreements appear on a
compact `also diverged at:` line so you can tell a one-off mismatch
from a fundamental disagreement.

```benchTranscript (caption := "lake exe bench compare MyApp.fastSort MyApp.slowSort")
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
on the return type so the harness can record hashes.

# Fixed-problem benchmarks

Sometimes you don't have a parameter to walk: there's a single hard
input and you want to record an absolute wall-clock number on it,
either to compare against an external reference (FLINT, fpLLL, …)
or to catch absolute-performance regressions over time. The
`setup_fixed_benchmark` macro registers a benchmark of this shape.

The function name *is* the benchmark name and must resolve to either
`α` (a pure value) or `IO α` (an effectful computation, useful when
the workload reads input from disk or shells out to an external
tool):

```lean
def heavyComputation : Nat := 42

setup_fixed_benchmark heavyComputation
```

There is no `Nat` parameter, no complexity expression, and no
verdict. The runner does one warmup call, then `repeats` measured
calls, and emits one JSONL row per repeat with `kind:"fixed"`. The
report shows median / min / max plus a hash-agreement check across
repeats — a divergence flags a non-deterministic registration.

`FixedBenchmarkConfig` knobs:

| Field | Default | Purpose |
|-------|---------|---------|
| `repeats` | `5` | Number of measured invocations after the warmup |
| `maxSecondsPerCall` | `60.0` | Hard wallclock cap per invocation (seconds) |
| `killGraceMs` | `100` | Grace ms between SIGTERM and SIGKILL |
| `warmup` | `true` | Whether to perform a single discarded warmup call |

The CLI flag `--repeats N` overrides the declared `repeats` per run;
`--max-seconds-per-call` is shared with parametric. Parametric-only
flags (`--param-ceiling`, `--slope-tolerance`, …) are silently
ignored when dispatching a fixed benchmark.

`compare A B` requires that all listed names be the same kind
(parametric or fixed). The fixed comparison report prints a
relative-timing line — each function's ratio against the first —
which is the use case for "this took ~2× FLINT."

# Caveats

- The wallclock cap is enforced via a pure-Lean kill path
  (`IO.Process.spawn` + `IO.Process.Child.kill`), so the harness
  works on every platform Lean's process API supports — Linux,
  macOS, and Windows — with no external `timeout(1)` dependency.
- The verdict is heuristic; the raw `ratios` array is the source of
  truth.
- Lean's `Nat` is arbitrary-precision. If your function returns big
  integers, the actual complexity includes the cost of arithmetic
  on results — likely worse than the "obvious" textbook complexity.
