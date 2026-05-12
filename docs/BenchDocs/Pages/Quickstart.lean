import VersoManual
import LeanBench
import BenchDocs.Block.Bench
import BenchDocs.Block.TempProj

open Verso.Genre Manual
open Verso.Genre.Manual.InlineLean
open BenchDocs.Block

#doc (Manual) "Quickstart" =>

A complete benchmark project in three files.

::: tempProj (tag := "myproject-3files")

```tempProjFile (path := "lakefile.toml")
name = "myproject"
defaultTargets = ["bench"]

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

# Build and run

The same project's `bench` exe supports `list`, `verify`, `run`, and
`compare` subcommands.

```bench (prog := "../.lake/build/bin/fib_benchmark_example") (argv := "list")
registered benchmarks:
  LeanBench.Examples.Fib.goodFib    expected complexity: n  [fib, linear]
  LeanBench.Examples.Fib.badFib    expected complexity: 144 ^ n / 89 ^ n  [fib, exponential]
```

`verify` is the fastest sanity-check: it invokes each registered
benchmark in-process against `f 0` and `f 1`, exits non-zero if any
check fails. There is no per-call kill on the in-process path —
verify-time inputs are tiny and a hang or native abort indicates a
real bug to fix in the bench, not the harness.

```bench (prog := "../.lake/build/bin/fib_benchmark_example") (argv := "verify")
verifying 2 benchmark(s)...
  [ok ] LeanBench.Examples.Fib.goodFib
  [ok ] LeanBench.Examples.Fib.badFib
all 2 benchmark(s) passed
```

`run` is the main event: it walks the parameter ladder, times each
rung, and prints a verdict. A wide-ladder run on `goodFib`:

```benchTranscript (caption := "lake exe fib_benchmark_example run goodFib")
$ lake exe fib_benchmark_example run goodFib --param-ceiling 4096 --max-seconds-per-call 1
LeanBench.Examples.Fib.goodFib    expected complexity: n    [warm cache]
  env: lean=4.30.0-rc2, os=linux, arch=x86_64, commit=<HASH>, <DATE>, host=<HOST>
  param  per-call    repeats  C
      0   46.000 ns  ×2^23    C= —
      1  107.000 ns  ×2^23    C= —
      2   82.000 ns  ×2^22    C=41.343 †
      4  112.000 ns  ×2^21    C= —     [<floor]
      8  200.000 ns  ×2^22    C=25.061 †
     16  278.000 ns  ×2^21    C=17.416
     32  459.000 ns  ×2^20    C=14.353
     64    1.236 µs  ×2^19    C=19.324
    128    2.334 µs  ×2^18    C=18.241
    256    3.743 µs  ×2^17    C=14.623
    512    7.506 µs  ×2^16    C=14.662
  1_024   17.287 µs  ×2^15    C=16.882
  2_048   18.792 µs  ×2^13    C= —     [<floor]
  4_096   43.304 µs  ×2^13    C=10.572
  verdict: consistent with declared complexity (cMin=10.572, cMax=19.324, β=-0.065)
  per-spawn floor (harness self-measurement): 33.998 ms
```

The shape — a `param` ladder, a per-rung `C` ratio, a verdict line
with `cMin`/`cMax`/`β`, and assorted decorations on individual
rows — is the
[Reading the output](https://kim-em.github.io/lean-bench/Quickstart/Reading-the-output/)
section's whole topic. Skip ahead if you want the table dissected
now; otherwise the next sections cover declaration-time options
and we'll meet the full output again later.

`compare` is the fourth subcommand and runs two or more named
benchmarks side by side, intersecting their common parameter values
and hashing every result so divergent implementations are flagged
explicitly. Covered in
[Compare divergence reports](https://kim-em.github.io/lean-bench/Quickstart/Compare-divergence-reports/).

# Optional per-param setup

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

setup_benchmark runBinary n => Nat.log2 (n + 1)
  with prep := prepInput
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

::: table +header
* * Asymptotic
  * Lean expression
* * O(1)
  * `1`
* * O(n)
  * `n`
* * O(n log n)
  * `n * Nat.log2 (n + 1)`
* * O(n²)
  * `n * n`
* * O(n³)
  * `n * n * n`
* * O(2ⁿ)
  * `2 ^ n`
:::

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

Fixed benchmarks support tags too:

```lean
def heavyComputation : Unit → Nat := fun () => 42

setup_fixed_benchmark heavyComputation where {
  tags := #["regression"]
  repeats := 10
}
```

All parent-side subcommands (`list`, `run`, `compare`, `verify`)
accept `--tag` and `--filter` flags:

::: table +header
* * Flag
  * Semantics
* * `--tag sort`
  * Benchmarks with the `sort` tag
* * `--tag sort,fib`
  * Benchmarks with `sort` OR `fib` (OR logic)
* * `--filter Sort`
  * Benchmarks whose name contains `Sort`
* * `--tag sort --filter Insert`
  * Both conditions must match (AND)
:::

When no `--tag` or `--filter` is given, all benchmarks are included.
Name filtering matches anywhere in the fully-qualified dotted name,
so `--filter Sort` matches `MyProject.Sort.runMergeSort`.

Concrete examples against the in-tree `fib_benchmark_example`, whose
two benchmarks are tagged `[fib, linear]` and `[fib, exponential]`:

```bench (prog := "../.lake/build/bin/fib_benchmark_example") (argv := "list --tag fib")
registered benchmarks:
  LeanBench.Examples.Fib.goodFib    expected complexity: n  [fib, linear]
  LeanBench.Examples.Fib.badFib    expected complexity: 144 ^ n / 89 ^ n  [fib, exponential]
```

```bench (prog := "../.lake/build/bin/fib_benchmark_example") (argv := "list --tag exponential")
registered benchmarks:
  LeanBench.Examples.Fib.badFib    expected complexity: 144 ^ n / 89 ^ n  [fib, exponential]
```

```bench (prog := "../.lake/build/bin/fib_benchmark_example") (argv := "list --filter goodFib")
registered benchmarks:
  LeanBench.Examples.Fib.goodFib    expected complexity: n  [fib, linear]
```

Lean namespaces naturally provide hierarchical grouping. Combined
with `--filter`, this gives you namespace-based filtering for free:

```lean
namespace MySort
  def runInsertionA (_ : Nat) : Nat := 0
  def runMergeSortA (_ : Nat) : Nat := 0
  setup_benchmark runInsertionA n => n * n
  setup_benchmark runMergeSortA n => n * Nat.log2 (n + 1)
end MySort

namespace MyFib
  def goodFibA (_ : Nat) : Nat := 0
  def badFibA  (_ : Nat) : Nat := 0
  setup_benchmark goodFibA n => n
  setup_benchmark badFibA  n => 144 ^ n / 89 ^ n
end MyFib
```

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

A budgeted run prints each benchmark's normal report as it lands,
plus a one-line budget summary at the bottom and a bullet per
skipped benchmark. A realistic 60-second suite with seven
parametric benchmarks tagged `regression`:

```benchTranscript (caption := "lake exe my_benchmarks run --tag regression --total-seconds 60")
$ lake exe my_benchmarks run --tag regression --total-seconds 60
MyProject.Hash.murmur3        expected complexity: n           [warm cache]
  env: lean=4.30.0-rc2, os=linux, arch=x86_64, commit=ab12cd, 2026-05-04T18:02:11Z, host=ci-runner-7
  param  per-call    repeats  C
     16   12.2 ns    ×2^21    C= —     [<floor] †
     32   24.1 ns    ×2^21    C= —     [<floor] †
     64   47.9 ns    ×2^21    C= 0.748
    128   95.0 ns    ×2^20    C= 0.742
    256    188 ns    ×2^19    C= 0.734
    512    374 ns    ×2^18    C= 0.730
  1_024    742 ns    ×2^17    C= 0.725
  2_048  1.49 µs    ×2^16    C= 0.727
  verdict: consistent with declared complexity (cMin=0.725, cMax=0.748, β=0.005)
  per-spawn floor (harness self-measurement): 1.4 ms

… five more benchmarks reported similarly …

MyProject.Sort.heapSort       expected complexity: n * Nat.log2 (n + 1)    [warm cache, budget truncated]
  param  per-call    repeats  C
     16    302 ns    ×2^16    C= 4.71 †
     32    684 ns    ×2^15    C= 4.27
     64  1.51 µs    ×2^14    C= 3.93
    128  3.32 µs    ×2^13    C= 3.71
  verdict: consistent with declared complexity (cMin=3.71, cMax=4.27, β=-0.038)
  per-spawn floor (harness self-measurement): 1.4 ms

budget: 60.000s; elapsed 58.910s; completed 7, skipped 2, truncated 1
  [budget skip] MyProject.Sort.runMergeSort    [parametric]
  [budget skip] MyProject.Sort.runInsertion    [parametric]
```

Each completed benchmark gets its full report (ladder + verdict +
per-spawn floor); the one whose ladder was cut off mid-flight is
tagged `[budget truncated]` on its header and stops at the last
rung that finished before the deadline. The two `[budget skip]`
bullets at the bottom name benchmarks that didn't get to start at
all. `completed 7` and `truncated 1` overlap: the truncated
benchmark is one of the seven that completed (it just stopped
early).

When `--export-file FILE` is also passed, the export document gains
a top-level `budget` object describing what happened. Each result
entry in `results[]` also carries `"budget_truncated": true|false`.

`run` and `compare` use distinct non-zero codes so CI can react
without parsing stdout:

::: table +header
* * code
  * meaning
* * `0`
  * success — the run completed and (with `--baseline`) no regressions were flagged
* * `1`
  * a regression vs. the baseline was flagged, or argument validation / dispatch errored
* * `2`
  * the parametric run produced *zero verdict-eligible rows*
:::

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

::: table +header
* * Flag
  * Type
  * `BenchmarkConfig` field
* * `--max-seconds-per-call`
  * Float
  * `maxSecondsPerCall`
* * `--target-inner-nanos`
  * Nat
  * `targetInnerNanos`
* * `--param-floor`
  * Nat
  * `paramFloor`
* * `--param-ceiling`
  * Nat
  * `paramCeiling`
* * `--warmup-fraction`
  * Float
  * `verdictWarmupFraction`
* * `--slope-tolerance`
  * Float
  * `slopeTolerance`
* * `--param-schedule`
  * ParamSchedule
  * `paramSchedule`
* * `--cache-mode`
  * CacheMode
  * `cacheMode`
* * `--outer-trials`
  * Nat
  * `outerTrials`
* * `--auto-fit`
  * (boolean flag)
  * (heuristic — see below)
:::

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

Each row of the report is a per-call wall time at one rung of the
ladder. The `C` column is the unit-cost ratio
`C = perCallNanos / complexity(param)`. When the declared
complexity matches reality, `C` is approximately constant across
the ladder; when it doesn't, `C` drifts. The verdict line is a
log-log slope check on `C` vs `param`: `|β| ≤ 0.15` reads
"consistent with declared complexity", anything wider reads
"inconclusive". β's sign tells you the direction — positive means
actually slower than declared, negative means actually faster.

The harness self-measures its per-spawn overhead and prints it
under the verdict as `per-spawn floor`. Any rung whose total
sample is shorter than `signalFloorMultiplier × spawn_floor`
(default 10×) is dominated by subprocess startup rather than the
function, so its per-call number is meaningless; those rows are
tagged `[<floor]` and dropped from the verdict.

If too few rungs land — every row is below the floor, every row
hits the cap, the ladder was truncated mid-flight, or fewer than
three rows survive the warmup-trim plus signal-floor filter — the
report can't compute a verdict, and `‼` advisory lines appear
naming concrete knobs to turn next (`--max-seconds-per-call`,
`--param-ceiling`, `paramSchedule := .custom #[...]`,
`setup_fixed_benchmark`, …).

A wide-ladder `goodFib` run, on stable hardware, gives a
consistent verdict. `†` flags the warmup-trim region (excluded
from the slope fit); `[<floor]` flags rungs dominated by spawn
overhead; the verdict line reports `cMin`, `cMax`, and `β`:

```benchTranscript (caption := "lake exe fib_benchmark_example run goodFib (consistent verdict)")
$ lake exe fib_benchmark_example run goodFib --param-ceiling 4096 --max-seconds-per-call 1
LeanBench.Examples.Fib.goodFib    expected complexity: n    [warm cache]
  env: lean=4.30.0-rc2, os=linux, arch=x86_64, commit=<HASH>, <DATE>, host=<HOST>
  param  per-call    repeats  C
      0   46.000 ns  ×2^23    C= —
      1  107.000 ns  ×2^23    C= —
      2   82.000 ns  ×2^22    C=41.343 †
      4  112.000 ns  ×2^21    C= —     [<floor]
      8  200.000 ns  ×2^22    C=25.061 †
     16  278.000 ns  ×2^21    C=17.416
     32  459.000 ns  ×2^20    C=14.353
     64    1.236 µs  ×2^19    C=19.324
    128    2.334 µs  ×2^18    C=18.241
    256    3.743 µs  ×2^17    C=14.623
    512    7.506 µs  ×2^16    C=14.662
  1_024   17.287 µs  ×2^15    C=16.882
  2_048   18.792 µs  ×2^13    C= —     [<floor]
  4_096   43.304 µs  ×2^13    C=10.572
  verdict: consistent with declared complexity (cMin=10.572, cMax=19.324, β=-0.065)
  per-spawn floor (harness self-measurement): 33.998 ms
```

`β=-0.065` sits inside the `|β| ≤ 0.15` band, so the verdict
reads "consistent". The table tells the same story: per-call time
grows roughly linearly with `param` once spawn overhead amortises,
and `C` stabilises near 15 ns. The two `[<floor]` rungs are
batches that ran shorter than 10× the spawn floor — pure noise,
and correctly dropped from the slope fit.

The opposite case: a tight ladder where the cap fires before any
real ladder data accumulates. The verdict reads "inconclusive"
and the `‼` advisories name the knob to turn:

```benchTranscript (caption := "lake exe fib_benchmark_example run goodFib (inconclusive — under-resolved)")
$ lake exe fib_benchmark_example run goodFib --param-ceiling 16 --max-seconds-per-call 0.5 --signal-floor-multiplier 1.0
LeanBench.Examples.Fib.goodFib    expected complexity: n    [warm cache]
  env: lean=4.30.0-rc2, os=linux, arch=x86_64, commit=<HASH>, <DATE>, host=<HOST>
  param  per-call    repeats  C
      0   55.000 ns  ×2^23    C= —
      1  113.000 ns  ×2^23    C= —
      2   79.000 ns  ×2^22    C=39.733
      4  140.000 ns  ×2^22    C=35.008
      8  234.000 ns  ×2^22    C=29.356
     16  247.000 ns  ×2^21    C=15.492
  verdict: inconclusive (cMin=15.492, cMax=39.733, β=-0.433, looks faster than declared by ~n^0.433)
  per-spawn floor (harness self-measurement): 32.899 ms
```

`β=-0.433` sits well outside the band, so the verdict reads
"inconclusive" with a directional hint ("looks faster than
declared by ~n^0.433"). It's a calibration failure, not a bug:
at small `n` the per-call setup dominates the linear algorithm,
dragging `C` down across rungs. Widening the ladder
(`--param-ceiling`) lets the asymptotic cost win and tips the
verdict back to consistent — exactly what the previous example
shows.

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

::: table +header
* * Catalog entry
  * What it represents
* * `1`
  * constant time
* * `n`
  * linear
* * `n * Nat.log2 (n + 1)`
  * linearithmic
* * `n^2`
  * quadratic
* * `n^3`
  * cubic
* * `2^n`
  * exponential
:::

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

The catalog labels are exactly the strings you would write after
`=>` in `setup_benchmark`, so adopting the suggestion is mechanical:

```lean
def myFibA (_ : Nat) : Nat := 0

-- Before
setup_benchmark myFibA n => 1
```

```lean
def myFibB (_ : Nat) : Nat := 0

-- After (auto-fit suggested `n`)
setup_benchmark myFibB n => n
```

This is a heuristic, not a proof. The catalog is fixed at six
entries, so anything not in the catalog gets the closest neighbour;
adjacent catalog entries (e.g. `n` vs `n * Nat.log2 (n + 1)`) are
hard to discriminate on short ladders. Read `stdLogC` to gauge how
good the closest match actually is; values above ~1.0 mean even the
winner doesn't fit well.

# Compare divergence reports

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
-- in real code: a single hard problem
def factorXOverFTwo : Unit → Nat := fun () => 42
setup_fixed_benchmark factorXOverFTwo

-- in real code: shell out to an external tool
def runFplll : IO Nat := pure 42
setup_fixed_benchmark runFplll where {
  repeats := 10
  maxSecondsPerCall := 30.0
}
```

There is no `Nat` parameter, no complexity expression, and no
verdict. The runner does one warmup call, then `repeats` measured
calls, and emits one JSONL row per repeat with `kind:"fixed"`. The
report shows median / min / max plus a hash-agreement check across
repeats — a divergence flags a non-deterministic registration.

`FixedBenchmarkConfig` knobs:

::: table +header
* * Field
  * Default
  * Purpose
* * `repeats`
  * `5`
  * Number of measured invocations after the warmup
* * `maxSecondsPerCall`
  * `60.0`
  * Hard wallclock cap per invocation (seconds)
* * `killGraceMs`
  * `100`
  * Grace ms between SIGTERM and SIGKILL
* * `warmup`
  * `true`
  * Whether to perform a single discarded warmup call
:::

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
