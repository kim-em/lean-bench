# lean-bench documentation site

This directory is the Verso source for the lean-bench docs at
<https://kim-em.github.io/lean-bench/>. It is its own Lake package
(separate from the main lean-bench package); it depends on lean-bench
by relative path, so no version of lean-bench is shipped to the
documentation reader other than the one in this checkout.

## Layout

```
docs/
├── lakefile.lean             — package definition (Verso v4.30.0-rc2)
├── lean-toolchain            — same toolchain as the parent
├── Main.lean                 — generate-bench-docs entry (manualMain)
├── BenchDocs.lean            — root #doc; aggregates all pages
├── BenchDocs/
│   ├── Block/                — custom Verso directives:
│   │   ├── Bench.lean         (`bench` and `benchTranscript`)
│   │   ├── TempProj.lean      (`::: tempProj` consumer-path harness)
│   │   ├── Normalise.lean     (timing/ratio/envJson/skipBlank line filters)
│   │   └── ExpectString.lean  (vendored line-diff helper)
│   ├── Pages/
│   │   ├── Quickstart.lean   — getting started, full user example
│   │   ├── Schema.lean       — JSONL wire format and export document
│   │   ├── Advanced.lean     — verdict, cache modes, outer trials, CI
│   │   ├── Pitfalls.lean     — Lean-specific benchmarking traps
│   │   ├── Profiling.lean    — perf/samply/heaptrack workflows
│   │   └── Design.lean       — architectural rationale (contributors)
│   └── Test/                 — focused unit tests for the directive
└── README.md                 — this file
```

## Building locally

Two modes. Both run `lake build` from inside `docs/` (NOT the parent).

### Editor mode (default)

```
$ cd docs
$ lake build
```

This compiles every page's prose and elaborates every `lean` /
`leanTerm` block against the live API, but does NOT spawn benchmark
processes. The directive's environment-variable gate
(`LEAN_BENCH_DOCS_CHECK`) is closed; `bench` and `tempProj` blocks
render their literal expected output without executing.

This is the right mode for prose authoring, fast iteration, and
running the Lean LSP. A fast workstation builds the whole site in
under 30 seconds from scratch and re-builds in seconds incrementally.

### Live-check mode (CI parity)

```
$ cd docs
$ LEAN_BENCH_DOCS_CHECK=1 lake build
```

This spawns the real binaries. Every `bench` block actually runs the
documented invocation (typically `fib_benchmark_example list` etc.)
from `../.lake/build/bin/`; every `::: tempProj` directive
materialises a complete consumer project in a temp dir and runs
`lake build` plus the documented bench invocation against it. The
expected output literal is diffed against the observed stdout
through whatever normalisers were declared.

This is what the GitHub Actions deploy workflow runs on every PR
(`.github/workflows/docs.yml`). It is also what you should run
locally before pushing if you've edited any `bench` or `tempProj`
block, because editor mode can't catch drift.

Live-check is slower: 11–15 s for the Quickstart page alone (the
TempProj harness fully builds a downstream lean-bench-using project
each time). If you only edited prose on one page, you can do a
targeted re-elaborate of just that page (see below).

### Cache invalidation gotcha

Lake caches elaboration by content hash, NOT by environment
variables. Toggling `LEAN_BENCH_DOCS_CHECK` between runs WILL NOT
re-elaborate cached pages; lake replays the previous result.

Workarounds:

```
# Force re-elaboration of one page:
$ rm -f .lake/build/lib/lean/BenchDocs/Pages/Quickstart.olean
$ LEAN_BENCH_DOCS_CHECK=1 lake build

# Or nuke the whole docs build cache:
$ rm -rf .lake/build/lib/lean/BenchDocs && LEAN_BENCH_DOCS_CHECK=1 lake build
```

CI is unaffected — every PR run starts from a fresh checkout, so
elaboration is unconditional.

### Generating the deployable site

```
$ LEAN_BENCH_DOCS_CHECK=1 lake exe generate-bench-docs --output _site --depth 2
```

Produces a multi-page tree under `_site/html-multi/`, with one
sub-page per top-level `#` heading inside each page. Open
`_site/html-multi/index.html` in a browser to view locally.

## Adding a new page

1. Create `BenchDocs/Pages/<Name>.lean`. Imports + page header:
   ```lean
   import VersoManual
   import LeanBench   -- only if you need `setup_benchmark` etc. in `lean` blocks
   import BenchDocs.Block.Bench
   import BenchDocs.Block.TempProj   -- only if you use `::: tempProj`

   open Verso.Genre Manual
   open Verso.Genre.Manual.InlineLean   -- for `lean` / `leanTerm` blocks
   open BenchDocs.Block

   #doc (Manual) "Title that becomes the page name in URLs" =>

   Body prose here.

   # Top-level heading becomes a sub-page

   …
   ```
2. Add `import BenchDocs.Pages.<Name>` and
   `{include 0 BenchDocs.Pages.<Name>}` to `BenchDocs.lean` so the
   root document picks it up.
3. `lake build` to confirm; `LEAN_BENCH_DOCS_CHECK=1 lake build` if
   the page contains any `bench` / `tempProj` blocks.

## Authoring blocks

### Checked Lean snippet

```
\`\`\`lean
def myFib : Nat → Nat
  | 0 => 0 | 1 => 1 | n + 2 => myFib n + myFib (n + 1)

setup_benchmark myFib n => 2 ^ n
\`\`\`
```

Elaborated against the page's accumulated environment (each block
shares state with previous blocks unless `(fresh := true)` is set).
Use unique identifiers per page when the same name would otherwise
be redeclared.

### Checked CLI invocation

```
\`\`\`bench (prog := "../.lake/build/bin/fib_benchmark_example") (argv := "list")
registered benchmarks:
  LeanBench.Examples.Fib.goodFib    expected complexity: n
  LeanBench.Examples.Fib.badFib    expected complexity: 144 ^ n / 89 ^ n
\`\`\`
```

Available config:
- `(prog := "...")` — required; resolved relative to the docs
  package's cwd (typically `docs/`). For repo-root binaries use
  `../.lake/build/bin/...`.
- `(argv := "space separated")` — split on whitespace; no shell
  semantics.
- `(cwd := "..")` — defaults to repo root. Set to `""` to use docs/.
- `(expectExit := 0)` — non-zero expected when the documented
  invocation is meant to fail.
- `(normalise := "timing,ratio,skipBlank")` — comma-separated list
  of normaliser names; see `BenchDocs/Block/Normalise.lean` for the
  available set.

### Unchecked transcript

```
\`\`\`benchTranscript (caption := "perf record output")
…long external profiler output…
\`\`\`
```

Rendered verbatim; not executed. Use for external profilers
(`perf`, `samply`, `heaptrack`), platform-specific tools, or any
other invocation that's inappropriate for CI. The visibly different
directive name signals to the reader that this output is
illustrative rather than verified.

### Multi-file consumer-path harness

```
::: tempProj (tag := "myproject")

\`\`\`tempProjFile (path := "lakefile.toml")
name = "myproject"
[[require]]
name = "lean-bench"
path = "{{LEAN_BENCH_PATH}}"
…
\`\`\`

\`\`\`tempProjFile (path := "MyProject.lean")
…
\`\`\`

\`\`\`tempProjRun (args := "bench list")
…expected stdout…
\`\`\`

:::
```

Materialises the declared files in a temp dir, substitutes
`{{LEAN_BENCH_PATH}}` for the absolute path to this lean-bench
checkout, runs `lake build`, runs `lake exe <args>`, diffs stdout
against `tempProjRun`'s body.

## Updating expected output

When the binary's output changes intentionally (a new flag, a
reformatted table, a renamed identifier), the doc's expected literal
will diverge from reality and CI will fail with a line-diff.

There are two ways to update:

1. **Manual.** Read the diff, edit the literal in the page source.
   Re-run live-check to confirm.
2. **Code-action suggestion.** When `expectString` flags a
   mismatch, it also emits a Verso `Suggestion` containing the new
   observed output. If your editor speaks Verso's suggestion
   protocol, you can accept it as a one-keystroke replacement.

For non-deterministic invocations (timings, hashes, env metadata),
add a normaliser instead of pinning the exact value:

```
\`\`\`bench ... (normalise := "timing,ratio,skipBlank")
       65_536  <TIME>     ×2^<N>  C=<NUM>
\`\`\`
```

`<TIME>`, `<N>`, `<NUM>` are placeholders the matching normaliser
expands at compare time.

## Running the directive's own tests

```
$ lake build test-bench-config-parse test-bench-diff test-bench-gate test-bench-normalise
$ lake exe test-bench-config-parse
$ lake exe test-bench-normalise
$ lake exe test-bench-gate              # reports CLOSED
$ LEAN_BENCH_DOCS_CHECK=1 lake exe test-bench-gate   # reports OPEN
```

The Normalise suite exercises 22 cases covering all four
normalisers; the others are smaller smoke tests. See
[`BenchDocs/Test/`](BenchDocs/Test/).

## Toolchain

`lean-toolchain` here pins `leanprover/lean4:v4.30.0-rc2`, which
matches the parent `lean-bench` package exactly. Verso is also
pinned to its `v4.30.0-rc2` tag in `lakefile.lean`. When the parent
toolchain bumps, both pins should be updated together; expect to
adapt to whatever Verso API churn happened in the meantime.
