import Cli
import LeanBench.Cli.Parse
import LeanBench.Cli.Handlers

/-!
# `LeanBench.Cli` — Cmd tree and `dispatch`

The user's `main` is just `LeanBench.Cli.dispatch`. The same compiled
binary serves as both parent (orchestrator) and child (single-batch
runner). Subcommand layout:

| invocation                                                      | role |
|-----------------------------------------------------------------|------|
| `./bench`                                                       | parent: print all registered benchmark names |
| `./bench list`                                                  | parent: same |
| `./bench run NAME [override flags]`                             | parent: run one or more benchmarks, print report |
| `./bench compare A B C [override flags]`                        | parent: comparison report |
| `./bench verify [NAMES…]`                                        | parent: bounded `f 0` / `f 1` sanity check via children |
| `./bench profile NAME --profiler "STR" [--param N]`              | parent: re-spawn child wrapped under user's profiler (issue #13) |
| `./bench _child --bench NAME --param N --target-nanos T`        | child: one inner-tuned batch, print one JSONL row, exit |

`run` and `compare` accept run-time `BenchmarkConfig` overrides via
flags (`--max-seconds-per-call`, `--target-inner-nanos`,
`--param-floor`, `--param-ceiling`, `--warmup-fraction`,
`--slope-tolerance`, `--param-schedule`). Each flag corresponds 1:1
with a `ConfigOverride` field; missing flags leave the declared
config untouched.

All parent-side subcommands accept `--tag TAG` and `--filter PATTERN`
for selecting benchmarks by tag or name substring. Tags are declared
via `where { tags := #["sort", "fast"] }` and matched by
comma-separated OR logic (`--tag sort,fast` matches benchmarks with
either tag). `--filter Sort` matches benchmarks whose name contains
"Sort" as a substring. See `doc/quickstart.md#tags-and-filtering`.

CLI parsing uses `Cli` (mhuisi/lean4-cli). The flag-parser plumbing
and override builders live in [`LeanBench.Cli.Parse`](Cli/Parse.lean);
the subcommand handler implementations live in
[`LeanBench.Cli.Handlers`](Cli/Handlers.lean).
-/

open Cli

namespace LeanBench
namespace Cli

/-! ## Cmd tree definitions -/

def listSub : Cmd := `[Cli|
  list VIA runListCmd; ["0.1.0"]
  "List registered benchmarks. Use --tag/--filter to narrow the output."

  FLAGS:
    tag : String;      "Filter by tag (comma-separated for multiple; OR logic). Example: --tag sort,fast"
    filter : String;   "Filter by name substring. Example: --filter Sort"
]

def runSub : Cmd := `[Cli|
  run VIA runRunCmd; ["0.1.0"]
  "Run registered benchmarks; print the result table.

Accepts explicit benchmark names as positional arguments, or use
--tag/--filter to select benchmarks by metadata.

Dispatches by registration kind (parametric vs fixed). Override flags
that don't apply to the dispatched kind are silently ignored, so
`--repeats` on a parametric benchmark and `--slope-tolerance` on a
fixed benchmark are both no-ops.

Override the benchmark's declared config per-run with the flags below.
Each missing flag leaves the declared value untouched."

  FLAGS:
    tag : String;      "Filter by tag (comma-separated for multiple; OR logic). Example: --tag sort,fast"
    filter : String;   "Filter by name substring. Example: --filter Sort"
    "max-seconds-per-call" : Float;  "Hard wallclock cap per batch / per call (seconds; e.g. 0.5; JSON-style numbers)."
    "target-inner-nanos" : Nat;      "Parametric only: auto-tune target wall-time per batch (nanoseconds)."
    "param-floor" : Nat;             "Parametric only: lowest param the doubling ladder starts from."
    "param-ceiling" : Nat;           "Parametric only: highest param the doubling ladder reaches before stopping."
    "warmup-fraction" : Float;       "Parametric only: fraction of leading ratios to drop before computing the verdict (e.g. 0.2; JSON-style numbers)."
    "slope-tolerance" : Float;       "Parametric only: verdict is `consistent` iff |β| ≤ this, where β is the log-log slope of C vs param (JSON-style numbers)."
    "param-schedule" : LeanBench.ParamSchedule;  "Parametric only: ladder shape (auto, doubling, or linear). Default auto picks doubling for polynomial growth, linear for exponential."
    "cache-mode" : LeanBench.CacheMode;           "Parametric only: warm (default) auto-tunes inner repeats inside one child; cold respawns per measurement so cache state is not preserved across rungs. See doc/advanced.md#cache-modes."
    "outer-trials" : Nat;            "Parametric only: number of independent outer trials per ladder rung (default 1). Bumping this above 1 runs N child spawns per param and reports per-param median / min / max / spread; trades runtime for stability. See doc/advanced.md#outer-trials."
    "signal-floor-multiplier" : Float; "Parametric only: per-spawn signal-floor multiplier (default 10.0; ≥ 1.0). Rows whose totalNanos is below `multiplier × spawnFloor` are flagged `[<floor]` and excluded from the verdict. `1.0` disables the filter; useful in CI smoke tests on slow runners. Issue #47."
    "auto-fit";                      "Parametric only: after the verdict, fit a fixed catalog of complexity models (1, n, n*log n, n^2, n^3, 2^n) to the observed per-call timings and print a ranked suggestion. Heuristic, not a proof. See doc/quickstart.md#auto-fit."
    "repeats" : Nat;                 "Fixed only: number of measured invocations after the warmup call (default 5)."
    "export-file" : String;           "Write results to FILE in machine-readable JSON format (issue #3)."
    baseline : String;               "Compare against a previous export FILE; report regressions and improvements. Exit code is non-zero when any regression exceeds the threshold."
    "regression-threshold" : Float;  "Percentage threshold for flagging regressions (default 10.0; e.g. 10 means >10% slower is a regression)."
    "total-seconds" : Float;         "CI-budget mode (issue #9): bound the whole suite to this many wallclock seconds. Benchmarks that would start past the deadline are recorded as 'budget skip' rows; a benchmark whose ladder runs into the deadline is truncated between rungs. Only meaningful with --tag/--filter (suite mode)."

  ARGS:
    ...names : String;     "Benchmark name(s). If omitted, use --tag/--filter to select."
]

def childSub : Cmd := `[Cli|
  _child VIA runChildCmd; ["0.1.0"]
  "Internal: child-mode single-batch runner. Not intended for direct use."

  FLAGS:
    bench : String;        "Benchmark name to dispatch."
    param : Nat;           "Parametric: parameter value to invoke the function with."
    "target-nanos" : Nat;  "Parametric: inner-tuning target wall-time (ns)."
    "cache-mode" : LeanBench.CacheMode;
                           "Parametric: warm (default — auto-tune inside this child) or cold (single untuned invocation; parent respawns per rung)."
    fixed;                 "Fixed: dispatch the fixed-benchmark single-invocation runner instead of the parametric autotuner."
    "repeat-index" : Nat;  "Fixed: 0-based repeat index to record on the emitted JSONL row."
    "env-json" : String;   "Issue #11: parent's pre-captured env JSON, propagated so all children stamp identical env on their rows. Falls back to fresh capture when absent or malformed."
]

def compareSub : Cmd := `[Cli|
  compare VIA runCompareCmd; ["0.1.0"]
  "Compare multiple registered benchmarks side-by-side.

All listed benchmarks must be the same kind (parametric or fixed);
mixing is rejected. Same per-run override flags as `run`; they apply
to every benchmark in the comparison.

Use --tag/--filter instead of explicit names to select benchmarks by
metadata. When both explicit names and filters are given, only the
explicit names are used."

  FLAGS:
    tag : String;      "Filter by tag (comma-separated for multiple; OR logic). Example: --tag sort,fast"
    filter : String;   "Filter by name substring. Example: --filter Sort"
    "max-seconds-per-call" : Float;  "Hard wallclock cap per batch / per call (seconds; e.g. 0.5; JSON-style numbers)."
    "target-inner-nanos" : Nat;      "Parametric only: auto-tune target wall-time per batch (nanoseconds)."
    "param-floor" : Nat;             "Parametric only: lowest param the doubling ladder starts from."
    "param-ceiling" : Nat;           "Parametric only: highest param the doubling ladder reaches before stopping."
    "warmup-fraction" : Float;       "Parametric only: fraction of leading ratios to drop before computing the verdict (e.g. 0.2; JSON-style numbers)."
    "slope-tolerance" : Float;       "Parametric only: verdict is `consistent` iff |β| ≤ this, where β is the log-log slope of C vs param (JSON-style numbers)."
    "param-schedule" : LeanBench.ParamSchedule;  "Parametric only: ladder shape (auto, doubling, or linear). Default auto picks doubling for polynomial growth, linear for exponential."
    "cache-mode" : LeanBench.CacheMode;           "Parametric only: warm (default) auto-tunes inner repeats inside one child; cold respawns per measurement so cache state is not preserved across rungs. See doc/advanced.md#cache-modes."
    "outer-trials" : Nat;            "Parametric only: number of independent outer trials per ladder rung (default 1). Bumping this above 1 runs N child spawns per param and reports per-param median / min / max / spread; trades runtime for stability. See doc/advanced.md#outer-trials."
    "signal-floor-multiplier" : Float; "Parametric only: per-spawn signal-floor multiplier (default 10.0; ≥ 1.0). Rows whose totalNanos is below `multiplier × spawnFloor` are flagged `[<floor]` and excluded from the verdict. `1.0` disables the filter; useful in CI smoke tests on slow runners. Issue #47."
    "auto-fit";                      "Parametric only: after each per-function verdict, fit a fixed catalog of complexity models to the observed timings and print a ranked suggestion. Heuristic, not a proof. See doc/quickstart.md#auto-fit."
    "repeats" : Nat;                 "Fixed only: number of measured invocations after the warmup call (default 5)."
    "export-file" : String;           "Write results to FILE in machine-readable JSON format (issue #3)."

  ARGS:
    ...names : String;     "Two or more benchmark names (variadic). Or use --tag/--filter to select."
]

def profileSub : Cmd := `[Cli|
  profile VIA runProfileCmd; ["0.1.0"]
  "Run a single benchmark invocation under an external profiler.

The harness re-spawns itself in child mode at one param, but with the
user-supplied --profiler command prefixed in front. The profiler's
output (perf record's perf.data, perf stat's summary, samply's
server URL, time -v's resource usage, …) lands directly on the user's
terminal alongside the child's JSONL row.

Single-shot by design: profile one param at a time. No ladder, no
verdict, no kill-on-cap (profilers can be slow to flush their own
output). The benchmark's declared cacheMode is honoured — pin
`--cache-mode cold` for one-shot first-touch profiling, leave it at
`warm` (default) so the autotuner exercises the function many times
inside one profiler invocation. See doc/profiling.md for end-to-end
workflows with perf, samply, heaptrack, and time -v.

Linux-first: --profiler is opaque, so any tool the user has on PATH
works, but the canned workflows in doc/profiling.md focus on perf
and samply on Linux. macOS users have Instruments and dtrace; Windows
users have ETW. The harness side is platform-neutral."

  FLAGS:
    profiler : String;               "Required. Profiler command prefix to wrap the child invocation (e.g. \"perf stat --\", \"perf record -F 99 -g --\", \"samply record --\", \"/usr/bin/time -v\"). Whitespace-split into argv tokens; no shell quoting."
    param : Nat;                     "Param value to invoke the function with. Default 0."
    "max-seconds-per-call" : Float;  "Override the declared per-batch cap (seconds). Note: there is no kill-on-cap in profile mode; this only flows through to the child's reporting."
    "target-inner-nanos" : Nat;      "Override the auto-tuner target wall-time per batch (ns). In warm mode (default) this controls how many iterations run inside one profiler invocation."
    "cache-mode" : LeanBench.CacheMode;  "warm (default — autotuned, many calls per profiler invocation) or cold (one untuned call per spawn)."

  ARGS:
    name : String;     "Benchmark name (as registered)."
]

def verifySub : Cmd := `[Cli|
  verify VIA runVerifyCmd; ["0.1.0"]
  "Sanity-check registered benchmarks (f 0, f 1 via the child path). Verifies all benchmarks if no names or filters are given. Exit code is non-zero on any failure."

  FLAGS:
    tag : String;      "Filter by tag (comma-separated for multiple; OR logic). Example: --tag sort,fast"
    filter : String;   "Filter by name substring. Example: --filter Sort"

  ARGS:
    ...names : String;     "Optional: benchmark names to verify; verify all if omitted."
]

/-- Top-level dispatcher; the user calls this as their `main`. -/
def topCmd : Cmd := `[Cli|
  bench NOOP; ["0.1.0"]
  "lean-bench: microbenchmarks for Lean 4. v0.1, Linux/macOS only."

  SUBCOMMANDS:
    listSub;
    runSub;
    compareSub;
    verifySub;
    profileSub;
    childSub
]

def dispatch (args : List String) : IO UInt32 :=
  let argv := if args.isEmpty then ["list"] else args
  topCmd.validate argv

end Cli
end LeanBench
