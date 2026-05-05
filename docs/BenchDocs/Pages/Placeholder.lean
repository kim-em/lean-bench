import VersoManual
import BenchDocs.Block.Bench

open Verso.Genre Manual
open Verso.Genre.Manual.InlineLean
open BenchDocs.Block

#doc (Manual) "Placeholder" =>

This page is a phase-1 placeholder; it exists to prove the doc-build
pipeline works end-to-end before any real content is migrated. It will
be deleted in phase 2 (the quickstart migration).

# Checked Lean snippet

The `lean` block below is elaborated against the live Lean compiler at
doc-build time. Any drift in the standard library — say, removing
`List.length` — would fail this build.

```lean
example : (List.range 5).length = 5 := rfl
```

# Checked CLI invocation

The `bench` block below runs the real `fib_benchmark_example` binary
when `LEAN_BENCH_DOCS_CHECK=1` is set in the environment, and diffs
its stdout against the literal block body. Without the env var (the
LSP / normal `lake build` case), the literal is rendered as-is and no
process is spawned.

```bench (prog := "../.lake/build/bin/fib_benchmark_example") (argv := "list")
registered benchmarks:
  LeanBench.Examples.Fib.goodFib    expected complexity: n
  LeanBench.Examples.Fib.badFib    expected complexity: 144 ^ n / 89 ^ n
```

# Unchecked transcript

The `benchTranscript` block is for invocations that aren't appropriate
for CI execution — external profilers, platform-specific tooling,
example output of long-running benchmarks. It renders verbatim with no
execution.

```benchTranscript (caption := "perf stat lake exe fib_benchmark_example run goodFib")
 Performance counter stats for 'lake exe fib_benchmark_example run goodFib':

         12,345.67 msec task-clock                #    1.000 CPUs utilized
                12      context-switches          #    0.972 /sec
                 0      cpu-migrations            #    0.000 /sec
            58,123      page-faults               #    4.708 K/sec
    34,567,890,123      cycles                    #    2.800 GHz
    87,654,321,098      instructions              #    2.54  insn per cycle

      12.346789012 seconds time elapsed
```
