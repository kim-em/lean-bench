module

public import Lean

public section

/-!
# `LeanBench.TimedRegions` — opt-in sidecar emission of timed region boundaries

External profilers (samply, perf, valgrind) sample whole processes;
they don't know which slice of process wallclock corresponds to the
bench library's *timed* regions vs. prep, autotune overhead between
probes, result hashing, or process startup/exit. When a downstream
tool wants to attribute samples only to the timed regions (e.g. to
build a kernel-only profile from a samply recording), it needs the
harness to tell it where those regions sit on the monotonic clock.

This module is profiler-agnostic: it just writes one JSONL record
per timed loop invocation to a sidecar file when the environment
variable `LEAN_BENCH_TIMED_REGIONS_SIDECAR` is set. A `%p` token in
the variable's value is replaced with the child PID, so a parent that
spawns several benchmark children can give each one a distinct file.
Downstream
tooling (an external postprocessor) consumes the sidecar plus the
profiler's own recording.

Design choices:

- **Env-var triggered.** No new CLI flags on `_child`; no new
  child mode. The harness's existing
  [`Profile`](Profile.lean) verb (opaque-profiler-prefix
  contract) stays the blessed entry point for users that don't
  need region attribution. Downstream packages set the env var
  before spawning the bench child.
- **Monotonic only.** The sidecar records `IO.monoNanosNow`
  values. Cross-clock alignment (e.g. mapping monotonic
  nanoseconds onto samply's wall-clock-derived sample
  timestamps) is the downstream tool's responsibility, because
  it has the profiler's own clock anchor and we don't.
- **Per-call records.** One JSONL line per call to the runner
  loop. The autotuner calls the loop several times with growing
  `count`; each call becomes one region. The downstream
  postprocessor unions the regions to define the filter
  window.
-/

namespace LeanBench

namespace TimedRegions

/-- Resolve the optional process-id token in a sidecar path template.
Replacing every `%p` matches common profiler path-template conventions
and makes one parent-provided environment value safe for many children. -/
def resolvePath (template : String) (pid : Nat) : String :=
  template.replace "%p" (toString pid)

/-- Render a string as a JSON string literal (including the surrounding
quotes). Delegates to `Lean.Json.str` for full RFC-8259 escaping,
including all control characters U+0000–U+001F that minimal
hand-rolled escape tables miss. -/
private def jsonString (s : String) : String :=
  (Lean.Json.str s).compress

/-- Render the header record. Written once per child, before any
region records. `clock_source` is informational for postprocessors;
the value `"CLOCK_MONOTONIC"` is true on Linux and macOS for Lean's
`IO.monoNanosNow` (it calls `clock_gettime(CLOCK_MONOTONIC)` on
both, and on macOS that is the same clock as `mach_absolute_time`
which is what samply records). -/
private def renderHeader (pid : Nat) (monoAnchorNs : Nat) : String :=
  "{" ++ String.intercalate "," [
    "\"kind\":\"header\"",
    s!"\"pid\":{pid}",
    "\"clock_source\":\"CLOCK_MONOTONIC\"",
    s!"\"mono_anchor_ns\":{monoAnchorNs}",
    "\"schema_version\":1"
  ] ++ "}\n"

/-- Render one region record. `label` is an opaque tag the caller
chooses; downstream postprocessors may filter by label (e.g. to
include only `"timed-loop"` regions and exclude any future
non-timing regions). -/
private def renderRegion
    (monoT0Ns monoT1Ns count : Nat) (label : String) : String :=
  "{" ++ String.intercalate "," [
    "\"kind\":\"region\"",
    s!"\"mono_t0_ns\":{monoT0Ns}",
    s!"\"mono_t1_ns\":{monoT1Ns}",
    s!"\"count\":{count}",
    s!"\"label\":{jsonString label}"
  ] ++ "}\n"

/-- Open the sidecar file at `path` (truncating any existing
content) and write the header record. The handle is returned and
left open for region records to append to. The caller is
responsible for closing the handle when emission is done.

Truncation rather than append is deliberate: if the parent reused
the path across runs (a tempfile typically isn't reused, but env-var
plumbing might surprise us), each child starts a clean record. -/
def openSidecar (path : String) : IO IO.FS.Handle := do
  let pid ← IO.Process.getPID
  let anchor ← IO.monoNanosNow
  let h ← IO.FS.Handle.mk (resolvePath path pid.toNat) .write
  h.putStr (renderHeader pid.toNat anchor)
  h.flush
  return h

/-- Append a region record. No flush — the caller is expected to
flush once at the end of all emission (typically in a `tryFinally`
that also closes the handle). Per-record flushing was found to add
profiler-visible non-region IO between short autotune probes for
no recovery benefit. -/
def writeRegion
    (h : IO.FS.Handle) (monoT0Ns monoT1Ns count : Nat) (label : String) :
    IO Unit :=
  h.putStr (renderRegion monoT0Ns monoT1Ns count label)

end TimedRegions

/-- Environment variable name. Opt-in; when unset, no sidecar
emission happens and the harness behaves exactly as before. -/
def timedRegionsEnvVar : String := "LEAN_BENCH_TIMED_REGIONS_SIDECAR"

/-- Wrap a `Nat → IO (Nat × Option UInt64)` loop closure so that each
invocation appends a region record to `h`. The wrapper records the
monotonic-clock boundaries *as close as possible* to the closure
call — i.e. just inside the wrapper, just outside the original
closure. The wrapped closure returns whatever the inner closure
returned.

Not thread-safe: assumes single-threaded use of the wrapped closure
(the harness's `runChildMode` autotuner is single-threaded). If the
helper is ever reused in a context that may call the wrapped loop
concurrently, callers must serialise around it. -/
def wrapLoopForSidecar
    (loop : Nat → IO (Nat × Option UInt64)) (h : IO.FS.Handle)
    (label : String) :
    Nat → IO (Nat × Option UInt64) := fun n => do
  let t0 ← IO.monoNanosNow
  let result ← loop n
  let t1 ← IO.monoNanosNow
  TimedRegions.writeRegion h t0 t1 n label
  return result

/-- Convenience: read the sidecar path from the environment, open the
file, wrap `loop`. Returns `(loop, some handle)` if the env var was
set, `(loop, none)` otherwise. The caller must close the handle when
emission is done by wrapping the call site in `tryFinally`. -/
def withSidecarIfEnabled
    (loop : Nat → IO (Nat × Option UInt64)) (label : String) :
    IO ((Nat → IO (Nat × Option UInt64)) × Option IO.FS.Handle) := do
  match ← IO.getEnv timedRegionsEnvVar with
  | none => return (loop, none)
  | some path =>
    let h ← TimedRegions.openSidecar path
    return (wrapLoopForSidecar loop h label, some h)

end LeanBench
