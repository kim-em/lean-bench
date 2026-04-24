import LeanBench

/-!
# Fib example

Three implementations of Fibonacci, all benchmarked side-by-side:

- `goodFib` — linear bottom-up, O(n)
- `badFib`  — naive double-recursive, O(2ⁿ)
- (FFI version deferred to v0.2; the FFI plumbing in `lakefile.toml`
  needs more attention than v0.1 wants to spend.)

Once you've got these registered with `setup_benchmark`, `lake exe
fib list` prints the catalogue and `lake exe fib run goodFib` runs
one of them. (Comparison is on the v0.1 roadmap inside this repo's
`PLAN.md`.)
-/

namespace LeanBench.Examples.Fib

/-- Linear Fibonacci, bottom-up. Tail-recursive accumulator. -/
def goodFib (n : Nat) : Nat :=
  let rec go (k : Nat) (a b : Nat) : Nat :=
    if k = 0 then a else go (k - 1) b (a + b)
  go n 0 1

/-- Doubly-recursive naive Fibonacci. O(2ⁿ). -/
def badFib : Nat → Nat
  | 0 => 0
  | 1 => 1
  | n + 2 => badFib n + badFib (n + 1)

-- Note: Lean's `Nat` is arbitrary-precision, so arithmetic on `Fib n`
-- (which is a number with ~0.7n bits) is *not* free. `goodFib n` does
-- `n` additions of numbers that grow like `Fib n`; each addition costs
-- O(n) bit operations; total is O(n²). Declaring complexity as `n`
-- would produce a growing `C`, which the verdict correctly flags as
-- inconclusive — the harness is doing its job.
setup_benchmark goodFib n => n * n

-- `badFib` has tree-recursive structure. Counting node work: O(Fib n),
-- which is Θ(φⁿ). We round to `2^n` — close enough for what the
-- verdict is checking.
setup_benchmark badFib  n => 2 ^ n

end LeanBench.Examples.Fib
