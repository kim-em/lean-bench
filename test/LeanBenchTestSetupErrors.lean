import LeanBench

/-!
# `setup_benchmark` static-error tests

Each `#guard_msgs` block confirms the macro fails with a helpful named
error. The elaborator's static checks (function-not-defined, bad
argument type, prep / function type mismatch) live here. The
compiled-code sanity checks (does `f 0` finish?) live in the
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

-- Test 3: `with prep := …` referencing an undefined prep.
def takesNat (_ : Nat) : Nat := 0

/--
error: setup_benchmark: function `LeanBench.Test.SetupErrors.noSuchPrep` is not defined
-/
#guard_msgs in
setup_benchmark takesNat n => n
  with prep := noSuchPrep

-- Test 4: prep doesn't take `Nat`.
def badPrep (_ : Bool) : Nat := 0

/--
error: setup_benchmark: function `LeanBench.Test.SetupErrors.badPrep` must take a single `Nat` argument; got `Bool`
-/
#guard_msgs in
setup_benchmark takesNat n => n
  with prep := badPrep

-- Test 5: function's argument type doesn't match prep's return type.
def prepReturnsString (_ : Nat) : String := ""

/--
error: setup_benchmark: function `LeanBench.Test.SetupErrors.takesNat` takes argument of type `Nat`, but prep `LeanBench.Test.SetupErrors.prepReturnsString` returns `String`
-/
#guard_msgs in
setup_benchmark takesNat n => n
  with prep := prepReturnsString

end LeanBench.Test.SetupErrors
