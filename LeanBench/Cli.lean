import Cli
import LeanBench.Core
import LeanBench.Env
import LeanBench.Run
import LeanBench.Child
import LeanBench.Compare
import LeanBench.Verify
import LeanBench.Format

/-!
# `LeanBench.Cli` — argv dispatcher

The user's `main` is just `LeanBench.Cli.dispatch`. The same compiled
binary serves as both parent (orchestrator) and child (single-batch
runner). Subcommand layout:

| invocation                                                      | role |
|-----------------------------------------------------------------|------|
| `./bench`                                                       | parent: print all registered benchmark names |
| `./bench list`                                                  | parent: same |
| `./bench run NAME`                                              | parent: run one benchmark, print report |
| `./bench compare A B C`                                         | parent: comparison report |
| `./bench verify [NAMES…]`                                        | parent: bounded `f 0` / `f 1` sanity check via children |
| `./bench _child --bench NAME --param N --target-nanos T`        | child: one inner-tuned batch, print one JSONL row, exit |

CLI parsing uses `Cli` (mhuisi/lean4-cli).
-/

open Cli

namespace LeanBench
namespace Cli

/-! ## Subcommand handlers (parent side) -/

def runListCmd (_ : Cli.Parsed) : IO UInt32 := do
  let entries ← allRuntimeEntries
  if entries.isEmpty then
    IO.println "(no benchmarks registered)"
    return 0
  IO.println "registered benchmarks:"
  for e in entries do
    IO.println (Format.fmtSpec e.spec)
  return 0

def runRunCmd (p : Cli.Parsed) : IO UInt32 := do
  let nameStr := (p.positionalArg! "name").as! String
  let result ← LeanBench.runBenchmark nameStr.toName
  IO.println (Format.fmtResult result)
  return 0

def runCompareCmd (p : Cli.Parsed) : IO UInt32 := do
  let names := p.variableArgsAs! String |>.toList
  if names.isEmpty then
    IO.eprintln "compare: need at least two benchmark names"
    return 1
  let report ← LeanBench.compare (names.map String.toName)
  IO.println (Format.fmtComparison report)
  return 0

def runVerifyCmd (p : Cli.Parsed) : IO UInt32 := do
  let names := p.variableArgsAs! String |>.toList |>.map Lean.Name.mkSimple
  match ← (LeanBench.verify names |>.toBaseIO) with
  | .error e =>
    IO.eprintln s!"verify: {e.toString}"
    return (1 : UInt32)
  | .ok reports =>
    IO.println (Format.fmtVerify reports)
    return if reports.all (·.passed) then (0 : UInt32) else (1 : UInt32)

/-! ## Subcommand handler (child side) -/

def runChildCmd (p : Cli.Parsed) : IO UInt32 := do
  let benchStr := (p.flag! "bench").as! String
  let param := (p.flag! "param").as! Nat
  let targetNanos := (p.flag! "target-nanos").as! Nat
  LeanBench.runChildMode benchStr.toName param targetNanos

/-! ## Cmd tree definitions -/

def listSub : Cmd := `[Cli|
  list VIA runListCmd; ["0.1.0"]
  "List registered benchmarks."
]

def runSub : Cmd := `[Cli|
  run VIA runRunCmd; ["0.1.0"]
  "Run a single registered benchmark; print the result table."

  ARGS:
    name : String;     "Benchmark name (lowercase, as registered)."
]

def childSub : Cmd := `[Cli|
  _child VIA runChildCmd; ["0.1.0"]
  "Internal: child-mode single-batch runner. Not intended for direct use."

  FLAGS:
    bench : String;        "Benchmark name to dispatch."
    param : Nat;           "Parameter value to invoke the function with."
    "target-nanos" : Nat;  "Inner-tuning target wall-time (ns)."
]

def compareSub : Cmd := `[Cli|
  compare VIA runCompareCmd; ["0.1.0"]
  "Compare multiple registered benchmarks side-by-side."

  ARGS:
    ...names : String;     "Two or more benchmark names (variadic)."
]

def verifySub : Cmd := `[Cli|
  verify VIA runVerifyCmd; ["0.1.0"]
  "Sanity-check registered benchmarks (f 0, f 1 via the child path). Verifies all benchmarks if no names are given. Exit code is non-zero on any failure."

  ARGS:
    ...names : String;     "Optional: benchmark names to verify; verify all if omitted."
]

/-- Top-level dispatcher; the user calls this as their `main`. -/
def topCmd : Cmd := `[Cli|
  bench NOOP; ["0.1.0"]
  "lean-bench: microbenchmarks for Lean 4. v0.1, Linux/macOS only."

  SUBCOMMANDS:
    listSub;
    runSub;
    compareSub;
    verifySub;
    childSub
]

def dispatch (args : List String) : IO UInt32 :=
  -- Default subcommand when invoked with no args: `list`.
  let argv := if args.isEmpty then ["list"] else args
  topCmd.validate argv

end Cli
end LeanBench
