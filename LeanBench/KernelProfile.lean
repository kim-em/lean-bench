module

public import LeanBench.Env
public import LeanBench.TimedRegions

public section

namespace LeanBench

/-- Profiling-only runners; ordinary scientific runners are unchanged. -/
abbrev KernelRunner := Nat → IO (Nat → IO (Nat × Option UInt64))

initialize kernelRegistry : IO.Ref (Std.HashMap Lean.Name KernelRunner) ← IO.mkRef {}

/-- Register the operation-only companion generated with each benchmark. -/
def registerKernel (name : Lean.Name) (runner : KernelRunner) : IO Unit :=
  kernelRegistry.modify (·.insert name runner)

/-- An opaque IO call keeps evaluation between the two profiling clock reads. -/
@[noinline] def invokeKernel (call : Unit → IO α) : IO α := call ()

/-- Build an operation-only profiling loop. Each result is retained through the
end clock, then fully consumed outside the emitted region. Sidecar writes and
result destruction are outside the region as well. The sum of kernel durations
drives the existing autotuner; it is not a scientific operation-plus-hash timing.
There is deliberately no batching of unconsumed results or growing result cache.
At most 100000 regions are emitted per child; smaller operations should use a
larger representative input, not generate an unbounded sidecar. -/
def kernelLoop (call : Unit → IO α) (consume : α → UInt64)
    (hashable : Bool) (firstHash : Bool := false) : IO (Nat → IO (Nat × Option UInt64)) := do
  let sidecar ← match ← IO.getEnv timedRegionsEnvVar with
    | some path => do pure (some (← TimedRegions.openSidecar path))
    | none => pure none
  let emitted ← IO.mkRef (0 : Nat)
  return fun count => do
    let mut total := 0
    let mut resultHash := none
    try
      for i in [0:count] do
        if sidecar.isSome && (← emitted.get) >= 100000 then
          throw (IO.userError "kernel profile region limit: choose a larger input or a shorter capture")
        let t0 ← IO.monoNanosNow
        let result ← invokeKernel call
        let t1 ← IO.monoNanosNow
        total := total + (t1 - t0)
        let digest := consume result
        blackBox digest
        if hashable && (!firstHash || i == 0) then resultHash := some digest
        if let some h := sidecar then
          TimedRegions.writeRegion h t0 t1 1 "kernel"
          emitted.modify (· + 1)
    finally
      if let some h := sidecar then h.flush
    return (total, resultHash)

end LeanBench
