import Lean.Data.Json
import LeanBench.Core

/-!
# `LeanBench.Schema` — versioned result schema

Single source of truth for the JSONL row format produced by the child
process and consumed by the parent. Pins the version constant, the
canonical required-key sets, and the tolerance rules described in
[`doc/schema.md`](../doc/schema.md).

The split from `LeanBench.Core` is deliberate: `Core` defines the
in-memory data shapes (`DataPoint`, `FixedDataPoint`, …) that are
internal to the harness, while `Schema` owns the wire format that
external tooling (baseline diffs, exporters, dashboards) reads. The
two move on different cadences: in-memory shapes can be refactored
freely; the wire format only changes under the rules in `schema.md`.
-/

open Lean (Json)

namespace LeanBench
namespace Schema

/-- Current schema version. Bump only on breaking changes — see
[`doc/schema.md#versioning`](../doc/schema.md#versioning). -/
def schemaVersion : Nat := 1

/-! ## Row-kind discriminator

Older parametric rows omit `kind` entirely. Readers treat a missing
`kind` as `"parametric"`; writers emit it explicitly going forward. -/

def kindParametric : String := "parametric"
def kindFixed      : String := "fixed"

/-! ## Canonical key sets

These are the keys readers and writers commit to. They drive the
schema-stability test in `test/LeanBenchTestSchema.lean`: a missing
key here vs. there means the diff caught a wire-format change that
needs human review. -/

/-- Required keys on every row, regardless of kind. -/
def requiredCommonKeys : Array String :=
  #["schema_version", "function", "status", "total_nanos"]

/-- Required keys on a parametric row, in addition to
    `requiredCommonKeys`. -/
def requiredParametricKeys : Array String :=
  #["param", "inner_repeats"]

/-- Required keys on a fixed row, in addition to
    `requiredCommonKeys`. -/
def requiredFixedKeys : Array String :=
  #["repeat_index"]

/-- Optional keys emitted on every row today. Absence is equivalent
    to `null`.

`alloc_bytes` and `peak_rss_kb` are the issue #6 memory metrics. They
are emitted on every row by current writers (so the schema-stability
test can assert their presence in the canonical key set), but each
field is platform-best-effort: on platforms or workloads where the
metric isn't available the writer emits JSON `null` rather than
omitting the key. Readers MUST tolerate either form. -/
def optionalCommonKeys : Array String :=
  #["kind", "result_hash", "error", "env", "alloc_bytes", "peak_rss_kb"]

/-- Optional keys emitted only on parametric rows today. -/
def optionalParametricKeys : Array String :=
  #["per_call_nanos", "cache_mode"]

/-! ## Environment-metadata sub-keys

The `env` value is itself a JSON object. `envKeys` pins the keys
its writer emits today. Like `optionalCommonKeys`, this exists so
the schema-stability test catches accidental drift in the field set
or its order.

Adding a new metadata field is a one-line edit here plus a matching
field on `LeanBench.Env`. Removing one is a wire-format break and
needs a `schemaVersion` bump (the field went from "writers may emit
it" to "writers don't emit it"; older readers are tolerant but
older *writers* against newer readers would still pass).

The order matches `LeanBench.RunEnv.toJson`'s emission order so the
schema test can do a strict array comparison after sorting.

Per issue #11: "Missing metadata is handled explicitly rather than
silently omitted." Producers MUST therefore emit every key — fields
the platform can't supply land as JSON `null`, never as an absent
key. Readers tolerate absence (treat it as `null`) for back-compat
with hand-rolled fixtures. -/
def envKeys : Array String :=
  #["lean_version", "lean_toolchain", "platform_target",
    "os", "arch", "cpu_model", "cpu_cores", "hostname",
    "exe_name", "lean_bench_version",
    "git_commit", "git_dirty",
    "timestamp_unix_ms", "timestamp_iso"]

/-! ## Cache-mode strings

Mirrors `CacheMode.toJsonString`. Pinned here so the schema-stability
test can assert the wire-format strings without depending on the
enum's `toJsonString` implementation. -/

def cacheModeWarm : String := "warm"
def cacheModeCold : String := "cold"

/-- Every documented cache-mode string. Readers MUST accept any of
    these as `CacheMode` values; producers MUST emit one of these
    on parametric rows that carry `cache_mode`. -/
def cacheModeStrings : Array String :=
  #[cacheModeWarm, cacheModeCold]

/-! ## Version handling

Reader contract: accept exactly the versions we know how to read.
There are no historical versions yet, so the admitted set is
`{schemaVersion}` plus a back-compat carve-out for missing
`schema_version`. -/

/-- Pull `schema_version` off a row. Missing → `none` (callers
    decide whether to default to `1` or fail). -/
def getVersion? (json : Json) : Option Nat :=
  match json.getObjValAs? Nat "schema_version" with
  | .ok n => some n
  | .error _ => none

/-- The set of `schema_version` values this reader can interpret.
    Currently a singleton; future readers add older versions here as
    they implement migrations. -/
def supportedVersions : Array Nat := #[schemaVersion]

/-- Validate that a row's `schema_version` is one we can read.

Rules:

- Missing `schema_version` is tolerated and treated as v1. This is
  the back-compat carve-out for hand-rolled fixtures and for any
  pre-versioning row that might still be in the wild.
- An explicit `schema_version` outside `supportedVersions` is
  rejected with an explicit error that names the version. We MUST
  NOT silently best-effort-read a future version (a renamed field
  would silently parse as `null`) or an older one we don't yet
  know how to migrate. -/
def checkVersion (json : Json) : Except String Unit := do
  match getVersion? json with
  | none => .ok ()
  | some v =>
    if supportedVersions.contains v then .ok ()
    else if v > schemaVersion then
      .error s!"schema_version {v} is newer than this reader (max {schemaVersion}); upgrade lean-bench to read it"
    else
      .error s!"schema_version {v} is older than this reader supports (supported: {supportedVersions}); migration not implemented"

/-- Read the `kind` discriminator. Default `"parametric"` when the
    field is missing — see the back-compat carve-out in
    [`doc/schema.md#compatibility`](../doc/schema.md#compatibility). -/
def getKind (json : Json) : String :=
  match json.getObjValAs? String "kind" with
  | .ok s => s
  | .error _ => kindParametric

/-- Validate that a row's `kind` matches `expected` (or is absent —
    treated as `"parametric"` per the back-compat rule). Used by both
    parsers to refuse rows that were emitted from the wrong path
    (e.g. a fixed row mis-routed into `parseChildRow`). -/
def checkKind (expected : String) (json : Json) : Except String Unit := do
  let actual := getKind json
  if actual == expected then .ok ()
  else .error s!"kind mismatch: expected {expected}, got {actual}"

/-! ## Status string contract

Mirrors `Status.toJsonString`. Pinning the canonical strings here
means the schema-stability test can assert the round-trip without
depending on `Status.toJsonString`'s implementation. -/

def statusOk          : String := "ok"
def statusTimedOut    : String := "timed_out"
def statusKilledAtCap : String := "killed_at_cap"
def statusError       : String := "error"

/-- Every documented status string. Readers MUST accept any of these
    as `Status` values; producers MUST emit one of these on every
    row. -/
def statusStrings : Array String :=
  #[statusOk, statusTimedOut, statusKilledAtCap, statusError]

end Schema
end LeanBench
