import LeanBench

/-!
# Sort example

Two sorting implementations on a deterministic random list of length
`n`, both registered with the right complexity model. `compare` shows
them side-by-side; the verdict tells you whether the observed scaling
matches the model.

- `insertionSort` — O(n²) insertion sort on a `List Nat`.
- `mergeSort` — `List.mergeSort` from core, O(n log n).

The "result" benchmarked is the head of the sorted list; that's the
observable we hash. (For real conformance you'd sum or hash the whole
list.)
-/

namespace LeanBench.Examples.Sort

/-- Insertion sort, O(n²). -/
def insertionSort (xs : List Nat) : List Nat :=
  xs.foldl (init := []) fun acc x =>
    let rec ins : List Nat → List Nat
      | [] => [x]
      | y :: ys => if x ≤ y then x :: y :: ys else y :: ins ys
    ins acc

/-- Generate a deterministic Nat list of length `n` from a SplitMix64-ish PRNG. -/
def gen (n : Nat) : List Nat :=
  (List.range n).map fun i =>
    let x : UInt64 := (i.toUInt64 + 0x9E3779B97F4A7C15) * 0xBF58476D1CE4E5B9
    x.toNat

/-- Benchmark target: insertion sort on a generated list, return its head. -/
def runInsertion (n : Nat) : Nat := (insertionSort (gen n)).headD 0

/-- Benchmark target: List.mergeSort on a generated list, return its head. -/
def runMergeSort (n : Nat) : Nat := ((gen n).mergeSort (· ≤ ·)).headD 0

setup_benchmark runInsertion n => n * n
setup_benchmark runMergeSort n => n * Nat.log2 (n + 1)

end LeanBench.Examples.Sort
