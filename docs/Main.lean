import BenchDocs
import VersoManual

open Verso.Genre.Manual

def main := manualMain (%doc BenchDocs) (config := config)
where
  config := {
    sourceLink := some "https://github.com/leanprover/lean-bench"
    issueLink := some "https://github.com/leanprover/lean-bench/issues"
    emitTeX := false
    emitHtmlSingle := .no
  }
