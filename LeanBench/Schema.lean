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
    to `null`. -/
def optionalCommonKeys : Array String :=
  #["kind", "result_hash", "error"]

/-- Optional keys emitted only on parametric rows today. -/
def optionalParametricKeys : Array String :=
  #["per_call_nanos", "cache_mode"]

/-! ## Suite-only optional keys (issue #9)

Emitted only on rows produced by `lean-bench suite --total-seconds`'s
JSONL exporter — never by the per-benchmark child path. They live in
their own bucket so the schema-stability test for child emit (which
pins the exact key set on a child row) doesn't have to know about
them. The unified suite-row test uses
`requiredCommonKeys ++ requiredParametricKeys ++ optionalCommonKeys
++ optionalParametricKeys ++ optionalSuiteKeys` for parametric, and
the analogous combination for fixed. -/

def optionalSuiteKeys : Array String :=
  #["budget_status"]

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

/-! ## Budget-status string contract (issue #9)

Pinned canonical strings for the `budget_status` field on suite-export
rows. Mirrors `BudgetStatus.toJsonString`. -/

def budgetStatusCompleted : String := "completed"
def budgetStatusSkipped   : String := "skipped"

/-- Every documented `budget_status` string. Producers MUST emit one
    of these on every row written by the suite-export path; readers
    MUST tolerate the field's absence (rows produced outside the
    suite path do not carry it) and reject unknown values. -/
def budgetStatusStrings : Array String :=
  #[budgetStatusCompleted, budgetStatusSkipped]

end Schema
end LeanBench
