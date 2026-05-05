import VersoManual
import BenchDocs.Block.Bench
import BenchDocs.Block.TempProj
import BenchDocs.Pages.Quickstart
import BenchDocs.Pages.Schema
import BenchDocs.Pages.Advanced
import BenchDocs.Pages.Pitfalls
import BenchDocs.Pages.Profiling
import BenchDocs.Pages.Design

open Verso.Genre Manual

#doc (Manual) "lean-bench documentation" =>

%%%
authors := ["Kim Morrison"]
shortTitle := "lean-bench"
%%%

Documentation for the [lean-bench](https://github.com/leanprover/lean-bench)
benchmarking harness for Lean 4.

{include 0 BenchDocs.Pages.Quickstart}

{include 0 BenchDocs.Pages.Schema}

{include 0 BenchDocs.Pages.Advanced}

{include 0 BenchDocs.Pages.Pitfalls}

{include 0 BenchDocs.Pages.Profiling}

{include 0 BenchDocs.Pages.Design}
