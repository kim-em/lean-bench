# Quickstart

A complete benchmark project in three files.

## 1. `lakefile.toml`

```toml
name = "myproject"
defaultTargets = ["MyProject"]

[[require]]
name = "lean-bench"
git = "https://github.com/kim-em/lean-bench.git"
rev = "main"

[[lean_lib]]
name = "MyProject"

[[lean_exe]]
name = "bench"
root = "MyProject.Bench"
```

## 2. `MyProject.lean`

```lean
def myFib : Nat → Nat
  | 0 => 0 | 1 => 1 | n + 2 => myFib n + myFib (n + 1)
```

## 3. `MyProject/Bench.lean`

```lean
import LeanBench
import MyProject

setup_benchmark myFib n => 2 ^ n

def main (args : List String) : IO UInt32 :=
  LeanBench.Cli.dispatch args
```

## Build and run

```bash
$ lake build bench
$ lake exe bench list
registered benchmarks:
  myFib    expected complexity: n + 1
$ lake exe bench run myFib
... result table ...
$ lake exe bench compare myFib someOtherFib
... comparison report ...
```

## Complexity expressions

The complexity expression on the right of `=>` has type `Nat → Nat`.
Pick whichever expression best models the algorithm:

| Asymptotic | Lean expression          |
|------------|--------------------------|
| O(1)       | `1`                      |
| O(n)       | `n`                      |
| O(n log n) | `n * Nat.log2 (n + 1)`   |
| O(n²)      | `n * n`                  |
| O(n³)      | `n * n * n`              |
| O(2ⁿ)      | `2 ^ n`                  |

`Nat.log2` is in core Lean. The `+ 1` in the `n log n` row guards
against `log2 0 = 0` producing a zero denominator in the ratio.

## Reading the output

Per-data-point ratio `C = perCallNanos / complexity(param)`. If your
declared complexity matches the implementation, `C` is approximately
constant. The verdict fits the log-log slope β of `C` vs `param` over
the trimmed tail: "consistent with declared complexity" when
`|β| ≤ 0.15`, otherwise "inconclusive". β is printed on the verdict
line; its sign tells you the direction of any mismatch (positive →
actually slower than declared, negative → actually faster).

The harness prints its own per-spawn floor as part of the report. Any
data point with `total_nanos` smaller than ~10× the spawn floor is
noise, not algorithm data — interpret it as a lower bound, not a
measurement.

## Caveats

- v0.1 has been tested only on Linux. The wallclock cap relies on
  GNU coreutils `timeout(1)` being in PATH, so macOS works if you
  `brew install coreutils`, WSL works, native Windows doesn't. A
  pure-Lean cross-platform replacement is on the v0.2 list (PLAN.md F0).
- The verdict is heuristic; the raw `ratios` array is the source of
  truth. See [PLAN.md](../PLAN.md) for v0.2 plans (auto-fit,
  baseline-diff, …).
- Lean's `Nat` is arbitrary-precision. If your function returns big
  integers, the actual complexity includes the cost of arithmetic on
  results — likely worse than the "obvious" textbook complexity.
