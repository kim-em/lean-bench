import BenchDocs
import VersoManual

open Verso.Genre.Manual

open Verso.Output.Html in
private def codeBlocksCss : Verso.Output.Html := {{
  <link rel="stylesheet" href="static/code-blocks.css" />
}}

open Verso.Output.Html in
private def codeBlocksJs : Verso.Output.Html := {{
  <script src="static/code-blocks.js" defer="defer"></script>
}}

def main := manualMain (%doc BenchDocs) (config := config)
where
  config := {
    sourceLink := some "https://github.com/leanprover/lean-bench"
    issueLink := some "https://github.com/leanprover/lean-bench/issues"
    emitTeX := false
    emitHtmlSingle := .no
    extraFiles := [("static", "static")]
    extraHead := #[codeBlocksCss, codeBlocksJs]
  }
