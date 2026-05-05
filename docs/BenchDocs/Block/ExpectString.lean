import Lean.Elab.Command
import Lean.Elab.InfoTree
import Verso

/-!
Vendored verbatim (with one-line attribution) from
`reference-manual/Manual/Meta/ExpectString.lean`. We vendor rather than
take a Verso/reference-manual dependency because the function is short
and self-contained, and the upstream lives in `Manual.Meta`, which is
not exported as a library.

Author of the vendored code: David Thrane Christiansen, Lean FRO LLC,
Apache 2.0 (see LICENSE in lean-bench).
-/

open Lean Elab
open Verso Doc

namespace BenchDocs.Block

variable {m : Type → Type} [Monad m] [MonadLog m] [AddMessageContext m] [MonadOptions m]
variable [MonadInfoTree m]

private def abbreviateString (what : String) (maxLength : Nat := 30) : String :=
  if what.length > maxLength then
    (what.take maxLength).copy ++ "…"
  else
    what

/--
Compares `expected` (taken from a string literal in the document source)
against `actual` (the observed output of a process or computation).

If the strings don't match, logs a line-by-line diff at the position of
`expected` and saves a code-action suggestion replacing `expected`'s
contents with `actual`. Lines are filtered through `useLine` and
compared modulo `preEq`. `what` describes the comparison in error
messages.

Errors are logged, not thrown; the returned `Bool` is `true` iff the
strings matched.
-/
def expectString (what : String) (expected : StrLit) (actual : String)
    (preEq : String → String := id)
    (useLine : String → Bool := fun _ => true) : m Bool := do
  let expectedLines := expected.getString.splitOn "\n" |>.filter useLine |>.toArray
  let actualLines   := actual.splitOn "\n"            |>.filter useLine |>.toArray

  unless expectedLines.map preEq == actualLines.map preEq do
    let diff := Diff.diff expectedLines actualLines
    logErrorAt expected m!"Mismatched {what}:\n{Diff.linesToString diff}"
    Suggestion.saveSuggestion expected (abbreviateString actual) actual
    return false

  return true

end BenchDocs.Block
