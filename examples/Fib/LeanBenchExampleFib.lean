import LeanBench

/-!
# Fib example

Two implementations of Fibonacci, both benchmarked.

- `goodFib` — linear bottom-up over `UInt64` (constant per-step).
   Declared complexity `n`.
- `badFib`  — naive doubly-recursive over `Nat`. Declared complexity
   `2 ^ n`. The verdict is `inconclusive` — the actual cost is
   `Θ(φ ^ n)` (golden ratio), and `2 ^ n` overestimates by a
   sub-exponential factor. The raw ratios show the gap.

Run them with `lake exe fib_benchmark_example list` /
`run NAME` / `compare A B`.
-/

namespace LeanBench.Examples.Fib

/--
Linear Fibonacci, bottom-up, `UInt64` to keep arithmetic
constant-per-step. Wraps mod 2^64 for large `n`; that's fine for a
benchmark, since we only care about timing not numeric value.
-/
def goodFib (n : Nat) : UInt64 := Id.run do
  let mut a : UInt64 := 0
  let mut b : UInt64 := 1
  for _ in [0:n] do
    let c := a + b
    a := b
    b := c
  return a

/-- Doubly-recursive naive Fibonacci over `Nat`. -/
def badFib : Nat → Nat
  | 0 => 0
  | 1 => 1
  | n + 2 => badFib n + badFib (n + 1)

setup_benchmark goodFib n => n
setup_benchmark badFib  n => 2 ^ n

end LeanBench.Examples.Fib
