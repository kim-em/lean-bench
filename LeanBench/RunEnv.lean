import Lean
import Std.Time
import LeanBench.Core

/-!
# `LeanBench.RunEnv` — capture reproducibility metadata (issue #11)

Two responsibilities:

1. **Capture.** Build an `Env` snapshot of the current process —
   Lean version, OS / arch, CPU model & core count, hostname,
   git commit & dirtiness, lean-bench version, executable name,
   wallclock timestamp.

2. **Encode.** Render an `Env` as a `Lean.Json` object suitable for
   embedding under the `env` key in a JSONL row, and as a one-line
   summary for human-readable reports.

`capture` is best-effort: every probe is wrapped so a failure on
one platform leaves that field as `none` rather than aborting
capture. The unit-style fields that are derivable from Lean
compile-time constants (`leanVersion`, `leanToolchain`,
`platformTarget`, `os`) are always populated.

The schema doc reserves the `env` key on JSONL rows for issue #11;
see [`doc/schema.md#environment-metadata`](../doc/schema.md#environment-metadata)
for the full field list and stability rules.
-/

open Lean (Json)

namespace LeanBench
namespace RunEnv

/-! ## OS / arch derivation -/

/-- Coarse OS family from `System.Platform`. `"unknown"` falls
through every platform check rather than a partial match — readers
should treat the value as informational, not authoritative. -/
def detectOs : String :=
  if System.Platform.isWindows then "windows"
  else if System.Platform.isOSX then "macos"
  else if System.Platform.isEmscripten then "emscripten"
  else "linux"

/-- Architecture parsed from the LLVM target triple's leading
segment. Returns `none` when the triple is empty or has no `-`. -/
def parseArch (target : String) : Option String :=
  if target.isEmpty then none
  else
    match target.splitOn "-" with
    | []      => none
    | a :: _  => if a.isEmpty then none else some a

/-! ## Best-effort probes

Each probe is tagged "best-effort": exceptions and non-zero exits
collapse to `none`. The motivating constraint is portability —
lean-bench runs on Linux, macOS, and Windows, and forcing a working
`/proc/cpuinfo` parse on every host would either hang Windows or
silently lie. -/

/-- Run a subprocess, capturing stdout. Returns `none` if the
process fails to spawn, exits non-zero, or emits an exception. The
trimmed stdout is returned on success; an empty stdout collapses to
`none` so callers don't have to special-case the "command exists
but produced nothing" case. -/
def tryRun (cmd : String) (args : Array String) : IO (Option String) := do
  try
    let out ← IO.Process.output { cmd, args, stdin := .null }
    if out.exitCode == 0 then
      let s := out.stdout.trimAscii.toString
      if s.isEmpty then pure none else pure (some s)
    else
      pure none
  catch _ => pure none

/-- Best-effort hostname: try `hostname` (POSIX), fall back to the
`HOSTNAME` / `COMPUTERNAME` env vars. Each step swallows errors. -/
def detectHostname : IO (Option String) := do
  match ← tryRun "hostname" #[] with
  | some h => return some h
  | none =>
    match ← (IO.getEnv "HOSTNAME") with
    | some h => if h.isEmpty then return none else return some h
    | none =>
      match ← (IO.getEnv "COMPUTERNAME") with
      | some h => if h.isEmpty then return none else return some h
      | none   => return none

/-- Read `/proc/cpuinfo` and pull the first `model name` field.
Linux-only; collapses to `none` on every other OS or when the file
is missing. We deliberately don't shell out to `sysctl` on macOS or
`wmic` on Windows to keep the probe quiet — that's a future
extension when there's a clear ask. -/
def detectCpuModel : IO (Option String) := do
  if System.Platform.isOSX || System.Platform.isWindows
     || System.Platform.isEmscripten then
    return none
  try
    let path : System.FilePath := "/proc/cpuinfo"
    if !(← path.pathExists) then return none
    let contents ← IO.FS.readFile path
    for line in contents.splitOn "\n" do
      if line.startsWith "model name" then
        match line.splitOn ":" with
        | _ :: rest =>
          return some (("".intercalate rest).trimAscii.toString)
        | _ => pure ()
    return none
  catch _ => return none

/-- Logical core count. Linux: count `processor` lines in
`/proc/cpuinfo`. macOS / Windows: best-effort env vars
(`NUMBER_OF_PROCESSORS`); falls back to `none`. -/
def detectCpuCores : IO (Option Nat) := do
  if System.Platform.isWindows then
    match ← IO.getEnv "NUMBER_OF_PROCESSORS" with
    | some s => return s.toNat?
    | none   => return none
  if System.Platform.isOSX || System.Platform.isEmscripten then
    return none
  try
    let path : System.FilePath := "/proc/cpuinfo"
    if !(← path.pathExists) then return none
    let contents ← IO.FS.readFile path
    let n := (contents.splitOn "\n").foldl
      (fun acc line => if line.startsWith "processor" then acc + 1 else acc) 0
    if n == 0 then return none else return some n
  catch _ => return none

/-- Git commit at cwd. We rely on the `git` binary being on PATH;
nothing in lean-bench's hot path depends on git, so a missing
binary just collapses to `none`. The output is the full 40-char
SHA — we don't truncate, since downstream tooling can do that. -/
def detectGitCommit : IO (Option String) := do
  match ← tryRun "git" #["rev-parse", "HEAD"] with
  | some s =>
    -- `git rev-parse` succeeds with an empty output in pathological
    -- cases; treat that as "no commit", same as a non-zero exit.
    if s.isEmpty then return none else return some s
  | none   => return none

/-- True iff `git status --porcelain` reports any change. `none`
when `git` itself is unavailable. -/
def detectGitDirty : IO (Option Bool) := do
  -- We use the same swallow-everything `tryRun` and key on whether
  -- the output is empty: `--porcelain` prints one line per dirty
  -- entry, nothing on a clean tree. A missing git binary or a
  -- non-repo cwd both collapse to `none` (distinct from
  -- `some false`).
  try
    let out ← IO.Process.output {
      cmd := "git", args := #["status", "--porcelain"], stdin := .null }
    if out.exitCode == 0 then
      return some (! out.stdout.trimAscii.toString.isEmpty)
    else
      return none
  catch _ => return none

/-- Basename of the running executable (e.g. `"fib_benchmark_example"`).
Strips both POSIX `/` and Windows `\\` separators so a Windows path
like `C:\\foo\\bar.exe` resolves cleanly. -/
def detectExeName : IO String := do
  let path := (← IO.appPath).toString
  let mut last : String := path
  for slice in path.split (fun c => c == '/' || c == '\\') do
    last := slice.toString
  return last

/-! ## Wallclock formatting -/

/-- Pad a `Nat` to width `w` with leading `0`s. Cheap helper for
ISO 8601 formatting — `2026 → "2026"`, `7 → "07"`, `123 → "123"`. -/
private def padNat (n : Nat) (w : Nat) : String :=
  let s := toString n
  if s.length ≥ w then s
  else String.ofList (List.replicate (w - s.length) '0') ++ s

/-- Render a wallclock instant (UTC) as ISO 8601: `YYYY-MM-DDTHH:MM:SSZ`.
Sub-second precision is dropped to keep the format readable in
report headers; the millisecond resolution is still available via
`timestampUnixMs` for tooling that wants it. -/
def fmtIso8601 (dt : Std.Time.PlainDateTime) : String :=
  let y : Int := dt.date.year
  let yStr := if y < 0 then "-" ++ padNat (Int.toNat (-y)) 4
              else padNat (Int.toNat y) 4
  let mo := padNat dt.date.month.val.toNat 2
  let d  := padNat dt.date.day.val.toNat 2
  let h  := padNat dt.time.hour.val.toNat 2
  let mi := padNat dt.time.minute.val.toNat 2
  let se := padNat dt.time.second.val.toNat 2
  s!"{yStr}-{mo}-{d}T{h}:{mi}:{se}Z"

/-! ## Capture -/

/-- Capture the current process's reproducibility metadata.

Best-effort: a single failed probe never aborts capture. The
returned `Env` is structurally complete — every field is either a
real value or an explicit `none`, never silently omitted (per the
issue #11 acceptance criterion). -/
def capture : IO Env := do
  let target  := System.Platform.target
  let exeName ← detectExeName
  let hostname ← detectHostname
  let cpuModel ← detectCpuModel
  let cpuCores ← detectCpuCores
  let gitCommit ← detectGitCommit
  let gitDirty ← detectGitDirty
  let ts ← Std.Time.Timestamp.now
  let unixMs : Int := (ts.toMillisecondsSinceUnixEpoch.val : Int)
  -- The `addSeconds 0` call is a no-op; the constructor just gives
  -- us a `PlainDateTime` in UTC without the local-zone lookup that
  -- `PlainDateTime.now` does. We want UTC for the ISO timestamp.
  let dt := Std.Time.PlainDateTime.ofTimestampAssumingUTC ts
  let iso := fmtIso8601 dt
  return {
    leanVersion       := Lean.versionString
    leanToolchain     := Lean.toolchain
    platformTarget    := target
    os                := detectOs
    arch              := parseArch target
    cpuModel          := cpuModel
    cpuCores          := cpuCores
    hostname          := hostname
    exeName           := exeName
    leanBenchVersion  := LeanBench.libraryVersion
    gitCommit         := gitCommit
    gitDirty          := gitDirty
    timestampUnixMs   := unixMs
    timestampIso      := iso
  }

/-! ## JSON encoding -/

/-- Render an `Option String` as `Json` (string or `null`). -/
private def jOptStr : Option String → Json
  | none   => Json.null
  | some s => Json.str s

/-- Render an `Option Nat` as `Json` (number or `null`). -/
private def jOptNat : Option Nat → Json
  | none   => Json.null
  | some n => Json.num (.fromNat n)

/-- Render an `Option Bool` as `Json` (bool or `null`). -/
private def jOptBool : Option Bool → Json
  | none   => Json.null
  | some b => Json.bool b

/-- Encode `Env` as a JSON object. The key order is fixed so the
schema-stability test can compare the keyset deterministically and
external dashboards can rely on diffability. -/
def toJson (e : Env) : Json :=
  Json.mkObj [
    ("lean_version",       Json.str e.leanVersion),
    ("lean_toolchain",     Json.str e.leanToolchain),
    ("platform_target",    Json.str e.platformTarget),
    ("os",                 Json.str e.os),
    ("arch",               jOptStr e.arch),
    ("cpu_model",          jOptStr e.cpuModel),
    ("cpu_cores",          jOptNat e.cpuCores),
    ("hostname",           jOptStr e.hostname),
    ("exe_name",           Json.str e.exeName),
    ("lean_bench_version", Json.str e.leanBenchVersion),
    ("git_commit",         jOptStr e.gitCommit),
    ("git_dirty",          jOptBool e.gitDirty),
    ("timestamp_unix_ms",  Json.num (.fromInt e.timestampUnixMs)),
    ("timestamp_iso",      Json.str e.timestampIso)
  ]

/-! ## Concise human-readable summary -/

/-- One-line summary used in report headers. Lean version, OS/arch,
git commit (truncated to 7 chars + dirty marker), and the ISO
timestamp. Hostname is appended only when present and non-empty. -/
def fmtConcise (e : Env) : String :=
  let archStr := e.arch.getD "?"
  let gitStr : String :=
    match e.gitCommit with
    | none   => "no-git"
    | some sha =>
      let short := sha.take 7 |>.toString
      let suffix : String :=
        match e.gitDirty with
        | some true  => "-dirty"
        | some false => ""
        | none       => "?"
      short ++ suffix
  let hostSuffix : String :=
    match e.hostname with
    | some h => s!", host={h}"
    | none   => ""
  s!"lean={e.leanVersion}, os={e.os}, arch={archStr}, commit={gitStr}, {e.timestampIso}{hostSuffix}"

end RunEnv
end LeanBench
