# Result schema

`lean-bench` emits machine-readable results as JSONL — one JSON object
per line — written to the child process's stdout and consumed by the
parent. Downstream tooling (baseline diffs, CI dashboards, exporters)
also reads these rows. This document pins the schema and spells out
the evolution rules so producers and consumers can rely on it.

The single source of truth in the code is
[`LeanBench/Schema.lean`](../LeanBench/Schema.lean): the
`schemaVersion` constant, the canonical key sets, and the parser
shared by `Run.parseChildRow` and `Run.parseFixedChildRow`.

## Row kinds

Two row kinds exist. They share most fields and the same
`schema_version` namespace.

- `parametric` — emitted by the parametric path (`setup_benchmark`).
  Carries one `(param, inner_repeats, total_nanos)` triple per child
  invocation.
- `fixed` — emitted by the fixed path (`setup_fixed_benchmark`).
  Carries one `(repeat_index, total_nanos)` triple per child
  invocation; no inner-repeat loop, no parameter.

Rows are tagged with `"kind":"parametric"` or `"kind":"fixed"`. Older
parametric rows produced before this field existed omit `kind`
entirely; readers MUST treat a missing `kind` as `"parametric"` to
preserve backwards compatibility (see [Compatibility](#compatibility)).

## Versioning

Every row carries a top-level integer `schema_version`. The current
version is **1**.

The version increments on any **breaking** change. Breaking changes
are:

- Removing a required field.
- Renaming a required field.
- Changing the type of a required field.
- Changing the meaning of an existing field (e.g. flipping units).
- **Adding a required field** (older writers won't emit it, so older
  data fails the new reader).
- **Promoting an existing optional field to required** (same reason).
- Tightening the value domain of a field in a way that rejects
  previously-valid data (e.g. making `result_hash` mandatory when it
  was optional).

The following changes are **non-breaking** (additive) and do NOT
bump the version:

- Adding a new **optional** field. Older readers ignore it; older
  writers still produce valid v1 rows.
- Documenting a previously-tolerated behaviour (e.g. accepting a
  shorter `result_hash`).
- Widening the value domain of an existing field (e.g. accepting more
  `status` strings).
- Making a previously-required field optional, provided readers continue
  to handle the field's absence as if the prior emit-side default
  were present.

Bumps are integer (`1 → 2 → 3`). There is no semver minor; consumers
key on the integer only.

## Required vs optional fields

### Required for both kinds

| key              | type    | meaning |
|------------------|---------|---------|
| `schema_version` | integer | Always `1` for v1 rows. |
| `kind`           | string  | `"parametric"` or `"fixed"`. Always emitted by current writers. Readers MUST reject a row whose `kind` mismatches the expected kind for the parser; a missing `kind` is tolerated only as `"parametric"` (the back-compat carve-out for parametric rows produced before this field was added). |
| `function`       | string  | Fully-qualified Lean name of the registered benchmark. |
| `total_nanos`    | integer | Wall time in nanoseconds for the measured work. |
| `status`         | string  | One of `"ok"`, `"timed_out"`, `"killed_at_cap"`, `"error"`. |

### Required for `parametric` only

| key              | type    | meaning |
|------------------|---------|---------|
| `param`          | integer | Ladder parameter the function was invoked with. |
| `inner_repeats`  | integer | Auto-tuned inner repeat count. `0` on synthesized error rows. |

### Required for `fixed` only

| key              | type    | meaning |
|------------------|---------|---------|
| `repeat_index`   | integer | 0-based index across the configured `repeats`. |

### Optional fields (both kinds)

These are emitted today. Readers MUST treat absence as if the field
were `null`.

| key                | type             | meaning |
|--------------------|------------------|---------|
| `result_hash`      | string \| null   | Hex-prefixed `0xDEADBEEF` literal (case-insensitive). `null` when the function's return type lacks `Hashable`. |
| `error`            | string \| null   | Free-form message for `status == "error"`. `null` otherwise. |
| `env`              | object \| null   | Reproducibility metadata captured at child startup — see [Environment metadata](#environment-metadata). Emitted on every row by current writers; absence is tolerated for back-compat with hand-rolled fixtures. |

`kind` is also classified as optional from the reader's standpoint
(absence defaults to `"parametric"`), but every current writer emits
it explicitly; producing a row without `kind` is a writer bug.

### Optional fields (parametric only)

| key                | type             | meaning |
|--------------------|------------------|---------|
| `per_call_nanos`   | number \| null   | `total_nanos / inner_repeats` precomputed. Derived; readers MAY recompute it from `total_nanos` and `inner_repeats` when absent. Emitted as `null` on synthesized error / killed-at-cap rows where `inner_repeats == 0` (a real division-by-zero), to keep every emitted row valid JSON. |
| `cache_mode`       | string           | `"warm"` (auto-tuned inner repeats inside one child, the v0.1 default) or `"cold"` (single untuned invocation; the parent respawns the child for every ladder rung). Absence is tolerated for back-compat with rows produced before issue #12 and treated as `"warm"`. See [`advanced.md#cache-modes`](advanced.md#cache-modes). |

### Reserved-for-future-use fields

The following keys are reserved by the issues that prompted this
schema and will land as additive (non-breaking) fields. They are
listed here so concurrent feature work claims a stable name from
the start:

- `alloc_bytes`, `peak_rss_kb` — memory metrics ([#6]).
- `budget_status` — additional status discriminator for budgeted-run
  rows that were skipped or partially completed ([#9]).

Producers MUST NOT emit any of these keys with semantics other than
the ones described in their landing PRs.

[#6]:  https://github.com/kim-em/lean-bench/issues/6
[#9]:  https://github.com/kim-em/lean-bench/issues/9
[#11]: https://github.com/kim-em/lean-bench/issues/11

## Environment metadata

The `env` field carries reproducibility metadata captured by the
child at startup (issue [#11]). It exists so a JSONL row is
self-describing for downstream tooling: which Lean version
produced this number? on what hardware? at which git commit?

The value is a JSON object with the following keys. Producers MUST
emit every key. Fields with no available value land as JSON `null`,
NOT as an absent key — the issue #11 acceptance criterion is
"Missing metadata is handled explicitly rather than silently
omitted." Readers MUST tolerate absent keys as `null` for
back-compat with hand-rolled fixtures.

| key                  | type            | meaning |
|----------------------|-----------------|---------|
| `lean_version`       | string          | `Lean.versionString` — e.g. `"4.30.0-rc2"`. |
| `lean_toolchain`     | string          | `Lean.toolchain` — matches the `lean-toolchain` file. |
| `platform_target`    | string          | LLVM target triple. Empty string when missing at compile time. |
| `os`                 | string          | Coarse OS family: `"linux"` / `"macos"` / `"windows"` / `"emscripten"`. |
| `arch`               | string \| null  | Architecture parsed from the target triple's leading segment. |
| `cpu_model`          | string \| null  | Linux: from `/proc/cpuinfo`. macOS / Windows: `null` (best-effort, not yet probed). |
| `cpu_cores`          | integer \| null | Logical core count. Linux: `/proc/cpuinfo`. Windows: `NUMBER_OF_PROCESSORS` env. |
| `hostname`           | string \| null  | `hostname(1)` or env-var fallback (`HOSTNAME` / `COMPUTERNAME`). |
| `exe_name`           | string          | Basename of the running executable. |
| `lean_bench_version` | string          | `LeanBench.libraryVersion`. |
| `git_commit`         | string \| null  | Full 40-char SHA from `git rev-parse HEAD` at cwd. `null` when git is unavailable. |
| `git_dirty`          | bool \| null    | True iff `git status --porcelain` was non-empty. `null` distinguishes "no git" from "clean tree". |
| `timestamp_unix_ms`  | integer         | Wallclock at capture, ms since Unix epoch. |
| `timestamp_iso`      | string          | Wallclock at capture, ISO 8601 UTC (`"YYYY-MM-DDTHH:MM:SSZ"`). |

The order writers emit these keys in is fixed by
`LeanBench.RunEnv.toJson` and pinned by the schema-stability test;
consumers MUST NOT depend on key order, but adding or removing a
key without updating both `Schema.envKeys` and the test fixture
will break the build.

The same env snapshot also rides on the in-memory
`BenchmarkResult` / `FixedResult` (parent-side capture) and is
surfaced as a one-line summary in human-readable reports.

## Compatibility

The contract for every reader and writer:

**Readers MUST:**

- Accept rows whose `schema_version` is in their `supportedVersions`
  set (currently the singleton `{1}`).
- Reject rows whose `schema_version` is outside that set with an
  explicit error that names the unsupported version. Silent
  best-effort reads of unknown versions risk producing wrong
  results.
- Tolerate a missing `schema_version` field by treating it as v1.
  This is the back-compat carve-out for hand-rolled fixtures and
  any pre-versioning row in the wild.
- Tolerate unknown extra keys: ignore them rather than failing.
- Tolerate missing optional keys (treating them as `null` / absent /
  the documented default).
- Treat a missing `kind` field as `"parametric"`. This is the one
  back-compat hatch for rows produced before `kind` was tagged on
  the parametric side. A present-but-mismatched `kind` (e.g. a
  fixed-shaped row arriving at the parametric parser) MUST be
  rejected.

**Readers MAY:**

- Decline to interpret rows whose required fields are missing or
  malformed — those are emitter bugs and surfacing them as a parse
  error is the right behaviour.

**Writers MUST:**

- Emit `schema_version` exactly once per row.
- Emit every required field for the row's kind.
- Use the documented status strings exactly. Unknown status strings
  are reserved for future use.

**Writers MAY:**

- Add new optional fields without bumping `schema_version`, provided
  every existing reader's "ignore unknown" rule keeps the output
  valid.
- Order the keys however they like. Consumers MUST NOT depend on key
  order.

## Migration

There are no historical schema versions yet — `schema_version == 1`
is the first formal cut. When the version bumps, this document will
gain a per-version section describing the migration. Readers
implementing migration should be permissive on input ("read what you
get") and strict on output ("emit the current version").

## Test enforcement

[`test/LeanBenchTestSchema.lean`](../test/LeanBenchTestSchema.lean)
pins:

- The exact `schema_version` constant.
- The canonical required-key sets for both row kinds.
- The parser's tolerance of unknown extra keys.
- The parser's tolerance of missing optional keys.
- The parser's rejection of missing required keys and unsupported
  schema versions.

Adding a new field is a one-line edit there. Removing a required
field requires a `schema_version` bump and a corresponding edit
to this document and the test fixtures, so the change shows up in
review.
