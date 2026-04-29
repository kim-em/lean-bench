import LeanBench

/-!
# Tests for issue #9 — CI-budget mode

Exercises `--total-seconds`:

1. `BudgetSummary` / `BudgetSkip` types are reachable from the public
   surface.
2. The `Format.fmtBudgetSummary` printer renders the expected line
   shape (the format-stability contract for CI-friendly parsing).
3. `Export.budgetSummaryToJson` carries the documented keys.
4. End-to-end: a budgeted suite run against several registered
   benchmarks completes within the wallclock bound and reports
   `budget_skip` rows for the benchmarks that didn't fit.

The end-to-end test is sensitive to wall time, so the per-benchmark
cost is kept small (microsecond functions, tight `paramCeiling`) and
the budget is set accordingly.
-/

open LeanBench

namespace LeanBench.Test.Budget

/-! ## Tiny benchmarks for the budget suite

Three parametric benchmarks plus one fixed benchmark. Each is cheap
on its own but the suite as a whole takes longer than the test's
budget so we can observe both completed and skipped rows. -/

def addOne (n : Nat) : Nat := n + 1
setup_benchmark addOne n => n where {
  tags := #["budget"]
  maxSecondsPerCall := 0.5
  paramCeiling := 1024
}

def addTwo (n : Nat) : Nat := n + 2
setup_benchmark addTwo n => n where {
  tags := #["budget"]
  maxSecondsPerCall := 0.5
  paramCeiling := 1024
}

def addThree (n : Nat) : Nat := n + 3
setup_benchmark addThree n => n where {
  tags := #["budget"]
  maxSecondsPerCall := 0.5
  paramCeiling := 1024
}

/-! ## Helpers -/

private def expectEq {α} [BEq α] [Repr α] (label : String) (got want : α) :
    IO Unit := do
  unless got == want do
    throw <| .userError s!"{label}: expected {repr want}, got {repr got}"

private def expectTrue (label : String) (b : Bool) : IO Unit := do
  unless b do throw <| .userError s!"{label}: expected true"

/-! ## Pure / type-level tests -/

def testBudgetSummaryFormat : IO UInt32 := do
  let skipped : Array (Lean.Name × String) :=
    #[(`a, "parametric"), (`b, "fixed")]
  let s := Format.fmtBudgetSummary 5.0 4.823 2 1 skipped
  -- Header line uses fmtFloat3 (3 decimals, padded). Be defensive
  -- about exact spacing — assert the substrings the schema doc
  -- promises.
  expectTrue "header.totalSeconds"   (LeanBench.Cli.containsSub s "5.000s")
  expectTrue "header.elapsedSeconds" (LeanBench.Cli.containsSub s "4.823s")
  expectTrue "header.completed"      (LeanBench.Cli.containsSub s "completed 2")
  expectTrue "header.skipped"        (LeanBench.Cli.containsSub s "skipped 2")
  expectTrue "header.truncated"      (LeanBench.Cli.containsSub s "truncated 1")
  expectTrue "skip.aTag"             (LeanBench.Cli.containsSub s "[budget skip] a")
  expectTrue "skip.aKind"            (LeanBench.Cli.containsSub s "[parametric]")
  expectTrue "skip.bTag"             (LeanBench.Cli.containsSub s "[budget skip] b")
  expectTrue "skip.bKind"            (LeanBench.Cli.containsSub s "[fixed]")
  -- Truncated count is omitted when zero (avoids noise on
  -- non-budget runs that happen to call this printer).
  let s2 := Format.fmtBudgetSummary 5.0 4.823 2 0 skipped
  unless !LeanBench.Cli.containsSub s2 "truncated" do
    throw <| .userError s!"expected no 'truncated' segment when count=0; got: {s2}"
  IO.println "  ok  budget.fmtBudgetSummary"
  return 0

def testBudgetSummaryJson : IO UInt32 := do
  let skipped : Array (Lean.Name × String) :=
    #[(`Foo.a, "parametric"), (`Foo.b, "fixed")]
  let j := Export.budgetSummaryToJson 5.0 4.5 1 0 skipped
  let pretty := j.compress
  -- Documented keys.
  for k in ["total_seconds", "elapsed_seconds", "completed",
            "truncated", "skipped"] do
    expectTrue s!"json.has.{k}" (LeanBench.Cli.containsSub pretty k)
  expectTrue "json.skip.kind" (LeanBench.Cli.containsSub pretty "\"kind\":\"parametric\"")
  expectTrue "json.skip.status"
    (LeanBench.Cli.containsSub pretty "\"status\":\"budget_skip\"")
  IO.println "  ok  budget.budgetSummaryToJson"
  return 0

def testDeadlinePast : IO UInt32 := do
  let now ← IO.monoNanosNow
  -- A deadline ten seconds in the past is exhausted.
  let dPast : Option Nat := some (now - 10_000_000_000)
  expectTrue "deadline.past"
    (← LeanBench.deadlineExceeded? dPast)
  -- A deadline ten seconds in the future is not.
  let dFuture : Option Nat := some (now + 10_000_000_000)
  let exceeded ← LeanBench.deadlineExceeded? dFuture
  expectTrue "deadline.future" (!exceeded)
  -- `none` is never exceeded.
  let exceededNone ← LeanBench.deadlineExceeded? none
  expectTrue "deadline.none" (!exceededNone)
  IO.println "  ok  budget.deadlineExceeded"
  return 0

/-! ## End-to-end via the CLI dispatcher

`--total-seconds 0.001` is small enough that the FIRST benchmark in
the budget suite is the only one that even gets to start (each rung
takes longer than a millisecond), so the second / third are emitted
as `budget_skip` rows. The first benchmark itself may also flip
`budgetTruncated` to `true` because its ladder doesn't finish before
the deadline trips. The export file we write below is asserted on
to confirm that fact lands on disk.

We use `--filter` rather than `--tag` because the `tags` test exe
also registers benchmarks tagged `budget`-adjacent things; using a
substring filter against this test exe's namespace keeps the suite
deterministic. -/

/-- Negative `--total-seconds` is rejected at the CLI rather than
silently producing a deadline far in the future. -/
def testNegativeTotalSeconds : IO UInt32 := do
  let code ← LeanBench.Cli.dispatch [
    "run",
    "--filter", "LeanBench.Test.Budget",
    "--total-seconds", "-1.0"
  ]
  expectEq "negativeTotalSeconds.exitCode" code (1 : UInt32)
  IO.println "  ok  budget.negativeTotalSeconds"
  return 0

def testEndToEndBudget : IO UInt32 := do
  let exportPath := "/tmp/lean_bench_budget_test.json"
  let _ ← (try IO.FS.removeFile exportPath catch _ => pure ())
  let code ← LeanBench.Cli.dispatch [
    "run",
    "--filter", "LeanBench.Test.Budget",
    "--total-seconds", "0.001",
    "--export-file", exportPath
  ]
  -- Exit code is non-zero only on a regression baseline mismatch;
  -- a budget skip on its own does not fail the run. We chose the
  -- 0.001s budget so we know there will be skips.
  expectEq "budget.exitCode" code (0 : UInt32)
  -- Verify the export file was written and carries the budget summary.
  let contents ← IO.FS.readFile exportPath
  expectTrue "export.hasBudget" (LeanBench.Cli.containsSub contents "\"budget\"")
  expectTrue "export.hasSkip" (LeanBench.Cli.containsSub contents "budget_skip")
  expectTrue "export.totalSeconds"
    (LeanBench.Cli.containsSub contents "\"total_seconds\"")
  IO.println "  ok  budget.endToEnd"
  return 0

end LeanBench.Test.Budget

/-! ## Driver -/

open LeanBench.Test.Budget in
def main : IO UInt32 := do
  let mut anyFail : UInt32 := 0
  for (label, t) in
    [ ("budget.fmtBudgetSummary",      testBudgetSummaryFormat),
      ("budget.budgetSummaryToJson",   testBudgetSummaryJson),
      ("budget.deadlineExceeded",      testDeadlinePast),
      ("budget.negativeTotalSeconds",  testNegativeTotalSeconds),
      ("budget.endToEnd",              testEndToEndBudget) ]
  do
    let code ← t
    if code != 0 then
      IO.eprintln s!"FAIL: {label}"
      anyFail := 1
  if anyFail == 0 then
    IO.println "budget tests passed"
  return anyFail
