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

/-! ## Row-kind discriminator -/

def kindParametric : String := "parametric"
def kindFixed      : String := "fixed"

/-! ## Canonical key sets

These are the keys readers and writers commit to. They drive the
schema-stability test in `test/LeanBenchTestSchema.lean`: a missing
key here vs. there means the diff caught a wire-format change that
needs human review. -/

/-- Required keys on every row, regardless of kind. -/
def requiredCommonKeys : Array String :=
  #["schema_version", "kind", "function", "status", "total_nanos"]

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
  #["result_hash", "error", "env", "alloc_bytes", "peak_rss_kb"]

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
silently omitted." Producers MUST emit every key — fields the
platform can't supply land as JSON `null`, never as an absent key. -/
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

Reader contract: accept exactly the versions we know how to read. -/

/-- The set of `schema_version` values this reader can interpret.
    Currently a singleton; future readers add older versions here as
    they implement migrations. -/
def supportedVersions : Array Nat := #[schemaVersion]

/-- Validate that a row's `schema_version` is one we can read. A
    `schema_version` outside `supportedVersions` is rejected with an
    explicit error that names the version: silently best-effort-reading
    a future version would let a renamed field parse as `null`, and
    older versions need explicit migrations. -/
def checkVersion (json : Json) : Except String Unit := do
  let v ← match json.getObjValAs? Nat "schema_version" with
    | .ok v => .ok v
    | .error _ => .error "missing or non-integer schema_version field"
  if supportedVersions.contains v then .ok ()
  else if v > schemaVersion then
    .error s!"schema_version {v} is newer than this reader (max {schemaVersion}); upgrade lean-bench to read it"
  else
    .error s!"schema_version {v} is older than this reader supports (supported: {supportedVersions}); migration not implemented"

/-- Validate that a row's `kind` matches `expected`. Used by both
    parsers to refuse rows that were emitted from the wrong path
    (e.g. a fixed row mis-routed into `parseChildRow`). -/
def checkKind (expected : String) (json : Json) : Except String Unit := do
  let actual ← match json.getObjValAs? String "kind" with
    | .ok s => .ok s
    | .error _ => .error "missing or non-string kind field"
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

/-- Parse a hex-prefixed UInt64 string like `"0xdeadbeef"`.

Rejects malformed input instead of silently coercing it: the body
must contain 1-16 hex digits and every digit must be in `[0-9a-fA-F]`.
-/
def parseHexU64 (s : String) : Except String UInt64 := do
  unless s.startsWith "0x" || s.startsWith "0X" do
    .error s!"invalid result_hash `{s}`: expected 0x-prefixed hex"
  let body := (s.drop 2).toString
  unless !body.isEmpty do
    .error s!"invalid result_hash `{s}`: missing hex digits"
  unless body.length ≤ 16 do
    .error s!"invalid result_hash `{s}`: more than 16 hex digits"
  let mut acc : UInt64 := 0
  for c in body.toList do
    let d? : Option UInt64 :=
      if c.isDigit then
        some (c.toNat - '0'.toNat).toUInt64
      else if 'a'.toNat ≤ c.toNat ∧ c.toNat ≤ 'f'.toNat then
        some (c.toNat - 'a'.toNat + 10).toUInt64
      else if 'A'.toNat ≤ c.toNat ∧ c.toNat ≤ 'F'.toNat then
        some (c.toNat - 'A'.toNat + 10).toUInt64
      else
        none
    let some d := d?
      | .error s!"invalid result_hash `{s}`: non-hex digit `{c}`"
    acc := acc * 16 + d
  return acc

end Schema
end LeanBench
