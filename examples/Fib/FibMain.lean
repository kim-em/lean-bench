import Fib

/-!
The user's `main` is one line. The `import Fib` brings the
`setup_benchmark` registrations along for the ride; the `initialize`
blocks emitted by the macro populate the runtime registry before
`main` runs.
-/

def main (args : List String) : IO UInt32 :=
  LeanBench.Cli.dispatch args
