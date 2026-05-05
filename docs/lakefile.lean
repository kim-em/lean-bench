import Lake
open Lake DSL

package docs where
  moreLeancArgs := #["-O0"]
  moreLinkArgs :=
    if System.Platform.isOSX then
      #["-Wl,-ignore_optimization_hints"]
    else #[]
  leanOptions := #[
    ⟨`weak.verso.code.warnLineLength, .ofNat 80⟩
  ]

require verso from git
  "https://github.com/leanprover/verso.git" @ "v4.30.0-rc2"
require versowebcomponents from git
  "https://github.com/leanprover/verso-web-components.git" @ "v4.30.0-rc2"
require leanBench from ".."

@[default_target]
lean_lib BenchDocs where

@[default_target]
lean_exe «generate-bench-docs» where
  root := `Main

lean_exe «test-bench-config-parse» where
  root := `BenchDocs.Test.ConfigParse

lean_exe «test-bench-diff» where
  root := `BenchDocs.Test.DiffUx

lean_exe «test-bench-gate» where
  root := `BenchDocs.Test.Gate

lean_exe «test-bench-normalise» where
  root := `BenchDocs.Test.Normalise
