import LeanBench

/-!
# BinarySearch example

Search for a target in a sorted array of length `n`. Both
implementations get the same input shape; binary search is `log n`
and linear scan is `n`. Compare them to see the harness's verdict
catch the asymptotic difference.

This example also demonstrates `with prep := …`. The array
construction is `O(n)`, which would dominate any `O(log n)` search if
it ran inside the timing loop. We register `prepInput` to build the
`(array, target)` pair once per param, outside the inner loop.
-/

namespace LeanBench.Examples.BinarySearch

/-- Sorted array of length `n` containing `[0, 1, 2, …, n-1]` (each
    multiplied by 7 to keep the values somewhat distinct from the
    indices). -/
def mkArr (n : Nat) : Array Nat :=
  (Array.range n).map (· * 7)

/-- Binary search; returns `some index` of the largest entry ≤ target,
    or `none` if the target is smaller than every entry. We always
    search for `target = (n-1)*7 / 2`, so the search succeeds. -/
partial def bsearch (a : Array Nat) (target : Nat) : Option Nat := Id.run do
  let mut lo := 0
  let mut hi := a.size
  let mut result : Option Nat := none
  while lo < hi do
    let mid := (lo + hi) / 2
    if a[mid]! ≤ target then
      result := some mid
      lo := mid + 1
    else
      hi := mid
  return result

/-- Linear scan: walk left-to-right, find the first index whose value
    exceeds target; the answer is one before that. -/
def linearScan (a : Array Nat) (target : Nat) : Option Nat := Id.run do
  let mut result : Option Nat := none
  for i in [0:a.size] do
    if a[i]! ≤ target then result := some i
  return result

/-- Per-param setup: build the sorted array and the search target. -/
def prepInput (n : Nat) : Array Nat × Nat :=
  (mkArr n, (n.max 1 - 1) * 7 / 2)

def runBinary (input : Array Nat × Nat) : Nat :=
  (bsearch input.1 input.2).getD 0

def runLinear (input : Array Nat × Nat) : Nat :=
  (linearScan input.1 input.2).getD 0

setup_benchmark runBinary n => Nat.log2 (n + 1)
  with prep := prepInput
setup_benchmark runLinear n => n
  with prep := prepInput

end LeanBench.Examples.BinarySearch

def main (args : List String) : IO UInt32 :=
  LeanBench.Cli.dispatch args

