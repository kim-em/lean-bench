/-!
# `LeanBench.MemStats` — memory metrics capture (issue #6)

Best-effort capture of process-level memory statistics, intended to be
called once at the end of a benchmark measurement so the harness can
report allocation / peak-RSS alongside wall time.

Two metrics are modelled:

- `peakRssKb` — peak resident-set-size of this process in kibibytes.
  Captured on Linux from `/proc/self/status` (`VmHWM:` line). `none`
  on every other platform; we deliberately don't shell out to
  `getrusage` / `task_info` / `GetProcessMemoryInfo` because doing so
  cross-platform would either require FFI or paying a subprocess
  spawn per measurement (which would itself dominate the metric for
  small benchmarks).

- `allocBytes` — total bytes allocated by the Lean runtime over the
  process lifetime. There is no portable in-process API for this in
  Lean 4 today, so the field is always `none`. It exists so the
  schema, parsers, exporters, and report renderers all have a place
  for the value to land once a future Lean release exposes it. See
  [`doc/schema.md#memory-metrics`](../doc/schema.md#memory-metrics)
  for the platform notes.

Both fields are optional on the wire format and treated as `null`
when absent (per the schema's "tolerate missing optional keys" rule).
A reader that doesn't care about memory metrics can ignore them
entirely.
-/

namespace LeanBench
namespace MemStats

/-- Captured memory statistics for a single child invocation.
Both fields are `Option` so a platform-unsupported metric is
distinguishable from a measured zero. -/
structure MemStats where
  /-- Total bytes allocated by the Lean runtime over the process
      lifetime. `none` on every platform today — there is no
      portable in-process API in Lean 4 to read this. The field
      exists so downstream consumers (and the wire schema) have a
      stable name to read once a future Lean release exposes
      allocation counters. -/
  allocBytes : Option Nat := none
  /-- Peak resident-set size in kibibytes. Captured on Linux from
      `/proc/self/status`'s `VmHWM:` line (which the kernel updates
      as the process's RSS grows). `none` on macOS, Windows, and
      any other platform without `/proc`. -/
  peakRssKb  : Option Nat := none
  deriving Repr, Inhabited

/-- Parse a `/proc/self/status` line of the form `VmHWM:    1234 kB`
    into the numeric kibibyte count. Returns `none` for anything
    that doesn't match (the kernel format is stable, but a future
    field reorder shouldn't crash the harness). -/
private def parseStatusKb (line : String) : Option Nat := Id.run do
  -- Strip everything up to and including the first colon.
  match line.splitOn ":" with
  | _ :: rest =>
    let payload := ("".intercalate rest).trimAscii.toString
    -- Drop the trailing `kB` (or anything non-numeric after the
    -- digits) by splitting on whitespace and taking the first token.
    let toks := payload.splitOn " "
    match toks with
    | t :: _ => return t.toNat?
    | _      => return none
  | _ => return none

/-- Best-effort peak-RSS read on Linux. Reads `/proc/self/status`
    once and pulls the `VmHWM:` field. Collapses every failure mode
    (file missing, permission denied, parse failure) to `none` so the
    metric is treated as "not available" rather than a crash. -/
private def detectPeakRssKbLinux : IO (Option Nat) := do
  try
    let path : System.FilePath := "/proc/self/status"
    if !(← path.pathExists) then return none
    let contents ← IO.FS.readFile path
    for line in contents.splitOn "\n" do
      if line.startsWith "VmHWM:" then
        return parseStatusKb line
    return none
  catch _ => return none

/-- Capture memory statistics for the current process. Called from
    the child at the end of a benchmark batch, before the JSONL row
    is written. -/
def capture : IO MemStats := do
  let peakRssKb ←
    if System.Platform.isWindows || System.Platform.isOSX
       || System.Platform.isEmscripten then
      pure none
    else
      detectPeakRssKbLinux
  return { allocBytes := none, peakRssKb }

end MemStats

/-- Re-export the structure at the `LeanBench` namespace so callers
    can write `LeanBench.MemStats` (the type) without the inner
    `MemStats.MemStats` repetition. -/
abbrev MemStats := MemStats.MemStats

end LeanBench
