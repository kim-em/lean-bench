import LeanBench

/-!
# `setup_benchmark` static-error tests

Each `#guard_msgs` block confirms the macro fails with a helpful named
error. v0.1 covers the two errors the elaborator can raise without
spawning a child process: function-not-defined, and function-arity-bad.
The compiled-code sanity checks (does `f 0` finish?) live in the
`lean-bench verify` subcommand and are tested by running it, not here.
-/

namespace LeanBench.Test.SetupErrors

-- Test 1: target function isn't defined anywhere.
/--
error: setup_benchmark: function `LeanBench.Test.SetupErrors.noSuchFunction` is not defined
-/
#guard_msgs in
setup_benchmark noSuchFunction n => n

-- Test 2: argument type isn't `Nat`.
def boolFn (b : Bool) : Nat := if b then 0 else 1

/--
error: setup_benchmark: function `LeanBench.Test.SetupErrors.boolFn` must take a single `Nat` argument; got `Bool`
-/
#guard_msgs in
setup_benchmark boolFn n => n

end LeanBench.Test.SetupErrors
