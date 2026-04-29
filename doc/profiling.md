# Profiling

Issue #13. Once `lake exe bench run` flags a regression, the next
question is *why*. lean-bench gives you a way to run a single
benchmark invocation under any external profiler — `perf`, `samply`,
`heaptrack`, `/usr/bin/time -v`, `valgrind --tool=callgrind`, … —
without writing shell glue.

The mechanism is intentionally minimal: the `profile` subcommand
re-invokes the benchmark binary in child mode at one parameter, but
with your profiler command prefixed in front. The profiler sees a
single short-lived child process that runs the registered function
many times (under the default `cacheMode := .warm` autotuner) and
then exits. Whatever output the profiler produces — flamegraphs,
counters, allocation traces — lands directly on your terminal or in
files at the location it chose.

This page documents four end-to-end workflows. None of them require
modifying your `setup_benchmark` declarations. All of them assume
you already have a populated benchmark binary; if not, see
[`quickstart.md`](quickstart.md) first.

## How `profile` works

```
$ lake exe bench profile NAME --profiler "PROFILER ARGS --" [--param N]
```

The harness assembles the argv:

```
PROFILER ARGS -- /path/to/bench _child --bench NAME --param N
                 --target-nanos T --cache-mode warm
```

…and `IO.Process.spawn`s the result with stdout / stderr inherited.
Differences from `run`:

- **Single param, no ladder.** The full doubling/linear sweep would
  produce N profile artifacts that collide on the same output
  filename. Pick the param you want to investigate (`--param 1024`)
  and re-run for each one.
- **No kill-on-cap.** Profilers can take seconds to flush their own
  output (`perf record` writes `perf.data`; `samply` boots a server).
  The wallclock cap that's right for benchmark mode would mis-fire.
- **No verdict reduction.** The child still emits its single JSONL
  row with the timing — useful for correlating profile artefacts
  against an absolute wall time — but no aggregation happens.
- **Profiler command is opaque.** The string you pass is
  whitespace-split into argv tokens; there is no shell-quoting layer.
  If your profiler invocation needs paths with spaces, wrap it in a
  one-liner script and pass the script's path.

The harness echoes the assembled command line to stderr before
exec'ing so you can see exactly what was run.

## Workflow 1 — `perf stat` (counters at a glance)

`perf stat` is the right tool when you want to know "is this a CPU
problem, a memory-bandwidth problem, or a branch-misprediction
problem?" before reaching for a profile. It has no output file —
results print to stderr at the end of the run.

```bash
$ lake exe bench profile myFib --param 4096 \
    --profiler "perf stat -e cycles,instructions,cache-misses,branch-misses --"
```

Expected output:

```
lean-bench profile: perf stat -e cycles,instructions,... -- /path/to/bench _child ...
{"schema_version":1,"kind":"parametric","function":"myFib","param":4096,...}

 Performance counter stats for ' /path/to/bench _child ... ':

   12,345,678,901      cycles
   18,234,567,890      instructions             #    1.48  insn per cycle
       12,345,678      cache-misses
        2,345,678      branch-misses

       0.512345678 seconds time elapsed
```

The benchmark's own JSONL row stays interleaved on stdout — the
`per_call_nanos` field gives you the per-iteration wall time, and
`inner_repeats` tells you how many calls the autotuner ran inside
this single profiler invocation.

## Workflow 2 — `perf record` + `perf report` / flamegraph

For "where is the hot code?" use `perf record`. It writes a
`perf.data` file in the current directory which `perf report` reads
back interactively, or which Brendan Gregg's
[FlameGraph](https://github.com/brendangregg/FlameGraph) tools
convert to SVG.

```bash
$ lake exe bench profile myFib --param 4096 \
    --profiler "perf record -F 999 -g --call-graph dwarf --"

# inspect interactively:
$ perf report

# or render a flamegraph (assumes FlameGraph/ on PATH):
$ perf script | stackcollapse-perf.pl | flamegraph.pl > myFib.svg
```

Notes:

- `-F 999` samples at 999 Hz, slightly off the kernel's default 1000
  Hz so samples don't lock-step with timer interrupts.
- `--call-graph dwarf` is more reliable than the default `fp` for
  Lean binaries because the Lean compiler doesn't always preserve
  frame pointers at `-O3`.
- `cacheMode := .warm` (the default) is what you want here: the
  autotuner runs the function ~thousands of times inside one `perf
  record` invocation, so the profile is dense.

## Workflow 3 — `samply` (perf-equivalent, cross-platform)

[`samply`](https://github.com/mstange/samply) is a perf-style
sampling profiler that works on Linux and macOS, with a built-in
Firefox-Profiler-compatible viewer. Useful when you don't want to
deal with `perf`'s output file or you're on macOS.

```bash
$ cargo install samply  # one-time
$ lake exe bench profile myFib --param 4096 \
    --profiler "samply record --"
```

`samply record` boots a local server and prints a URL; open it in a
browser for the interactive flamegraph view.

On macOS, `samply` is the path of least resistance — Apple's
Instruments works too, but its CLI integration is heavier.

## Workflow 4 — `/usr/bin/time -v` (resource usage / RSS)

Issue #13 mentions allocation profilers; the lightest-weight
proxy on Linux is the GNU `time -v` flag, which reports peak
resident-set size, page faults, voluntary / involuntary context
switches, and several other kernel resource counters.

```bash
$ lake exe bench profile myFib --param 1048576 \
    --profiler "/usr/bin/time -v"
```

Expected output (abridged):

```
        Maximum resident set size (kbytes): 184320
        Major (requiring I/O) page faults: 0
        Minor (reclaiming a frame) page faults: 12345
        Voluntary context switches: 7
        Involuntary context switches: 23
```

For deeper allocation inspection, `heaptrack` is the natural next
step on Linux:

```bash
$ lake exe bench profile myFib --param 1024 \
    --profiler "heaptrack --"
$ heaptrack_gui heaptrack.bench.NNNN.gz
```

`heaptrack` produces one trace per spawn, named with the child PID,
so multi-param sweeps don't collide.

## Picking the right cache mode

The benchmark's declared `cacheMode` flows through to the child:

- **`cacheMode := .warm` (default)** — the autotuner picks an inner
  repeat count and runs the function many times in one process.
  This is what you want for `perf record` / `samply` (dense profile)
  and for `perf stat` (steady-state counters).
- **`cacheMode := .cold`** — the child runs the function exactly
  once. This is what you want when you specifically care about
  first-call costs: cache-cold paths, lazy initialization, JIT-style
  patterns. The downside is that the profile is very thin —
  consider `--cache-mode cold` plus a longer `perf record` run, or
  use a different profiler altogether (e.g., `bpftrace` watching a
  uprobe).

Pin from the CLI per run:

```bash
$ lake exe bench profile myFib --param 1024 \
    --cache-mode cold --profiler "perf record -F 999 -g --"
```

## What `profile` does not do

- It does not run the verdict / advisory pipeline. If you want a
  verdict-eligible measurement, use `run` and `compare`.
- It does not parse the profiler's output. Whatever the tool prints
  is what you see. This is the point: the harness gets out of the
  way once it has spawned the wrapped child.
- It does not aggregate across runs. A profile of one param at a
  time is the contract; loop in your shell if you need many.
- It is not a replacement for `verify`, `run`, or `compare`. It is
  an *additional* entry point for the case where you've already
  identified something worth investigating.

## Platform notes

- Linux is the canonical target. `perf` requires
  `/proc/sys/kernel/perf_event_paranoid` ≤ 2 (the kernel default on
  most distros) and is most useful with kernel symbols available.
- macOS users should reach for `samply` first; Apple's Instruments
  works (the CLI is `xcrun xctrace record`) but the UX is heavier
  and the results aren't as portable.
- Windows users have ETW via `xperf` / Windows Performance Recorder.
  The `--profiler` flag is opaque, so any of these tools work — the
  harness side does not care.

## Acceptance criteria status

- ✓ Users can invoke benchmarks in profiler-friendly mode without
  custom shell glue: `lake exe bench profile NAME --profiler "..."`.
- ✓ Documentation includes at least one end-to-end profiling
  workflow: this page documents four (`perf stat`, `perf record`,
  `samply`, `/usr/bin/time -v` + `heaptrack`).
- ✓ The feature does not distort ordinary benchmark mode behavior:
  `profile` is a separate subcommand that imports `LeanBench.Profile`;
  nothing in `LeanBench.Run` / `LeanBench.Stats` / `LeanBench.Compare`
  changed. `run` / `compare` / `verify` measurement paths are
  byte-for-byte unchanged.
