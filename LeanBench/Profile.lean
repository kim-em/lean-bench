import Lean
import LeanBench.Core
import LeanBench.Run

/-!
# `LeanBench.Profile` — wrap a single child invocation under an external profiler

The benchmark harness measures *that* something is slow. Profiling
tells you *why*. This module is the minimum plumbing that lets a user
run one ladder rung under any external profiler (`perf`, `samply`,
`heaptrack`, `time -v`, `valgrind`, `dtrace`, …) without writing shell
glue: the parent re-invokes itself in child mode, but with a
user-supplied profiler-command prefix in front of the executable.

Design choices, all driven by the issue #13 acceptance criteria:

- **Profiler command is opaque.** We don't try to abstract perf vs
  samply vs valgrind behind one interface — each tool's CLI is its
  own dialect (`-o`, `-F`, `--call-graph`, `--`). The user passes the
  full prefix as a single string; we whitespace-split it and exec.
  This deliberately punts on shell quoting (paths with spaces won't
  round-trip) — see the `profileTokens` docstring for the contract.

- **No killer task.** Profilers can be slow to flush their own output
  files (`perf record` writes `perf.data`, can take seconds on a big
  recording). The wallclock cap that's right for benchmark mode
  would mis-fire here.

- **Single param, single child.** A full ladder produces N profile
  artifacts that almost certainly collide on the same output filename
  unless the user threaded `$param` through their command. Doing one
  rung per `profile` invocation keeps the contract simple and matches
  every profiler workflow the author has seen.

- **stdout / stderr inherit.** The child still emits its single JSONL
  row to stdout; the profiler writes its own report to stdout / stderr
  / a side file. The user sees both. We don't try to capture and
  parse — that's what `run` is for.

- **No effect on `run` / `compare` / `verify`.** This module adds a
  fresh entry point; nothing in the existing measurement path
  changes. That's the third acceptance criterion ("does not distort
  ordinary benchmark mode behavior") — by construction, since the
  ordinary paths don't import this module's functions.
-/

namespace LeanBench

/-- Whitespace-split the profiler command into argv tokens.

    Contract: we split on ASCII spaces / tabs / newlines and drop
    empty tokens. There is no shell-quoting layer — a path containing
    a space won't round-trip. Users who need quoting can wrap their
    profiler in a tiny script and pass the script's path here, or use
    a profiler that takes a single positional path argument and put
    that argument in a known location.

    Examples:
    - `"perf stat --"`           → `#["perf", "stat", "--"]`
    - `"perf record -F 99 -g --"` → `#["perf", "record", "-F", "99", "-g", "--"]`
    - `"  samply  record  --"`   → `#["samply", "record", "--"]`
    - `""`                       → `#[]` (caller treats as "no profiler") -/
def profileTokens (cmd : String) : Array String :=
  cmd.splitOn " " |>.foldl (init := (#[] : Array String)) fun acc raw =>
    -- Inner trim handles the tab / newline case (`cmd.split` only
    -- splits on space; an embedded tab from a here-doc would survive
    -- as part of a token and confuse the spawned profiler).
    let tok := raw.trimAscii.toString
    if tok.isEmpty then acc else acc.push tok

/-- Build the argv that wraps the child invocation under the profiler.

    Returns `(cmd, args)` ready for `IO.Process.spawn`. The first
    token of the user-supplied profiler command becomes `cmd`; every
    remaining token plus the harness's own `_child` argv becomes
    `args`. When the profiler command is empty the function returns
    `none` and the caller emits a user-facing error. -/
def buildProfileArgv (profilerCmd : String) (exe : String)
    (childArgs : Array String) : Option (String × Array String) :=
  let toks := profileTokens profilerCmd
  if h : 0 < toks.size then
    let head := toks[0]
    let rest := toks.extract 1 toks.size
    some (head, rest ++ #[exe] ++ childArgs)
  else
    none

/-- Construct the `_child` argv the profile path would invoke. Pulled
    out of `runProfile` so the argv assembly is unit-testable without
    actually spawning anything. Mirrors `Run.runOneBatch`'s argv with
    one omission: no `--env-json`, because we don't want to thread a
    huge JSON blob through `perf record`'s argv when the child can
    capture it itself in ~3ms. The profile path is the one place
    where re-capturing env in the child is cheaper than serializing
    it through. -/
def profileChildArgs (spec : BenchmarkSpec) (param : Nat) : Array String :=
  #[ "_child"
   , "--bench", spec.name.toString (escape := false)
   , "--param", toString param
   , "--target-nanos", toString spec.config.targetInnerNanos
   , "--cache-mode", spec.config.cacheMode.toJsonString ]

/-- Run a single child invocation wrapped in the user's profiler
    command. Inherits stdout / stderr so the profiler's output and
    the child's JSONL row both land on the user's terminal.

    Returns the wrapped process's exit code. A non-zero exit may come
    from either the profiler (`perf` returns the wrapped command's
    code in most modes, but exits non-zero on its own permission /
    setup failures) or the child; we don't try to disambiguate. The
    caller surfaces the code as the parent process's exit code. -/
def runProfile (name : Lean.Name) (param : Nat) (profilerCmd : String)
    (override : ConfigOverride := {}) : IO UInt32 := do
  let some entry ← findRuntimeEntry name
    | throw (.userError s!"unregistered benchmark: {name}")
  let cfg := override.apply entry.spec.config
  match cfg.validate with
  | .error msg => throw (.userError s!"{name}: {msg}")
  | .ok () => pure ()
  let spec := { entry.spec with config := cfg }
  let exe ← ownExe
  let childArgs := profileChildArgs spec param
  match buildProfileArgv profilerCmd exe childArgs with
  | none =>
    -- The CLI handler guards against empty profilerCmd before reaching
    -- here, so this branch is defensive only. Kept so direct callers
    -- of `runProfile` (test fixtures, downstream tooling) get a clean
    -- error instead of an empty-argv `IO.Process.spawn` panic.
    throw (.userError "profile: --profiler must be a non-empty command (e.g. \"perf stat --\")")
  | some (cmd, args) =>
    -- Echo the wrapped command to stderr so the user sees exactly what
    -- gets exec'd. Useful when the profiler is misconfigured ("perf:
    -- not found") or when debugging what argv the harness actually
    -- assembled.
    let argLine := String.intercalate " " args.toList
    IO.eprintln s!"lean-bench profile: {cmd} {argLine}"
    let child ← IO.Process.spawn {
      cmd := cmd
      args := args
      -- Inherit so the JSONL row lands on stdout next to whatever the
      -- profiler prints. Some profilers (`perf record`) write to
      -- stderr; some (`perf stat`) write a summary to stderr; some
      -- (`samply`) write a server URL. Treating all of them as
      -- opaque text streams means we don't need a per-tool adapter.
      stdout := .inherit
      stderr := .inherit
      stdin  := .null
    }
    let exit ← child.wait
    return exit

end LeanBench
