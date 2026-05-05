import BenchDocs.Block.Bench

/-! Sanity-build target for the diff-rendering path. The actual diff
quality is covered by the integration test in
`BenchDocs.Pages.Placeholder` — running `LEAN_BENCH_DOCS_CHECK=1
lake build` after deliberately drifting the placeholder's expected
output produces a line-by-line diff in the build error.

This exe is a no-op; its existence verifies that the directive
module compiles standalone and is wired into Lake. -/

def main : IO UInt32 := do
  IO.println "DiffUx exe built; behavioural diff coverage lives in the placeholder integration test"
  return 0
