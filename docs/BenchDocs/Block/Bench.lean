import Lean.Elab.Command
import Verso
import Verso.Doc.ArgParse
import Verso.Doc.Elab.Monad
import VersoManual
import BenchDocs.Block.ExpectString
import BenchDocs.Block.Normalise

/-!
The `bench` and `benchTranscript` code-block directives. Modeled on
`reference-manual/Manual/Meta/LakeCheck.lean`.

`bench` runs an external binary and diffs the observed output (after
normalisation) against the literal block body. Live execution is gated
on the `LEAN_BENCH_DOCS_CHECK` env var so the editor's LSP does not
spawn benchmark processes on every keystroke.

`benchTranscript` is a render-only directive for invocations that
shouldn't run in CI (external profilers, platform-specific tooling).
The literal body is shown verbatim with no execution and no checking.

Both directives render the literal block body as a code block, so the
reader sees what the author wrote regardless of execution mode.
-/

open Lean Elab
open Verso ArgParse Doc Elab Genre.Manual

namespace BenchDocs.Block

variable {m : Type → Type}
variable [Monad m] [MonadInfoTree m] [MonadLiftT CoreM m] [MonadEnv m] [MonadError m]

/-! ## Argument plumbing -/

structure BenchConfig where
  /-- The executable to run. Resolved relative to the parent process's
  cwd (Lake's invocation directory, which for the docs package is
  `docs/`). For repo-root-relative paths, prefix with `../`. -/
  prog       : String
  /-- Space-separated argv (no shell parsing — embedded spaces aren't
  supported). -/
  argv       : String := ""
  /-- Optional display form rendered in the prompt line. Defaults to
  `lake exe <basename> <argv>` when `prog` looks like
  `…/.lake/build/bin/<basename>`; otherwise `<prog> <argv>`. -/
  display    : String := ""
  /-- Working directory of the spawned process, relative to the parent's
  cwd. Defaults to the lean-bench repo root. -/
  cwd        : String := ".."
  /-- Expected exit code. Anything else fails the directive. -/
  expectExit : Nat := 0
  /-- Comma-separated list of normaliser names: `timing`, `ratio`,
  `envJson`, `skipBlank`. -/
  normalise  : String := ""

private def parseNormalisers (s : String) : Except String (Array Normaliser) := Id.run do
  if !s.toList.any (fun c => !c.isWhitespace) then return .ok #[]
  let parts : Array String :=
    s.splitOn "," |>.map (fun p => splitOnWhitespace p) |>.flatten |>.toArray
  let mut out : Array Normaliser := #[]
  for p in parts do
    if p.isEmpty then continue
    match p with
    | "timing" => out := out.push .timing
    | "ratio"  => out := out.push .ratio
    | "envJson" => out := out.push .envJson
    | "skipBlank" => out := out.push .skipBlank
    | other =>
      return .error s!"Unknown normaliser '{other}' (expected one of: timing, ratio, envJson, skipBlank)"
  return .ok out
where
  splitOnWhitespace (s : String) : List String :=
    -- A poor-man's whitespace split: replace any run of whitespace with a single
    -- space, then `splitOn " "` and drop empties.
    let cs := s.toList.map (fun c => if c.isWhitespace then ' ' else c)
    let collapsed := String.ofList cs
    collapsed.splitOn " " |>.filter (!·.isEmpty)

/-- Split a `String` of argv tokens on ASCII whitespace; drop empties. -/
def splitArgv (s : String) : Array String :=
  let cs := s.toList.map (fun c => if c.isWhitespace then ' ' else c)
  String.ofList cs |>.splitOn " " |>.filter (!·.isEmpty) |>.toArray

private def BenchConfig.parser : ArgParse m BenchConfig :=
  BenchConfig.mk
    <$> .named  `prog       .string false
    <*> .namedD `argv       .string ""
    <*> .namedD `display    .string ""
    <*> .namedD `cwd        .string ".."
    <*> .namedD `expectExit .nat    0
    <*> .namedD `normalise  .string ""

instance : FromArgs BenchConfig m := ⟨BenchConfig.parser⟩

/-! ## Execution -/

private def gateEnabled : IO Bool := do
  return (← IO.getEnv "LEAN_BENCH_DOCS_CHECK").isSome

/-- The basename of `prog`'s path: text after the last `/`. -/
private def basename (path : String) : String :=
  let parts := path.splitOn "/"
  parts.getLast?.getD path

/-- Build the prompt line shown above the output, given the directive's
config. If `display` is explicitly set, use it. Otherwise derive a
`lake exe <basename> <argv>` form when `prog` lives under
`.lake/build/bin/`, else `<prog> <argv>`. -/
private def buildPromptLine (cfg : BenchConfig) : String :=
  if !cfg.display.isEmpty then
    "$ " ++ cfg.display
  else if cfg.prog.contains '/' && (".lake/build/bin/").isPrefixOf
      (cfg.prog.replace "../" "") then
    let exe := basename cfg.prog
    let argv := if cfg.argv.isEmpty then "" else " " ++ cfg.argv
    "$ lake exe " ++ exe ++ argv
  else
    let argv := if cfg.argv.isEmpty then "" else " " ++ cfg.argv
    "$ " ++ cfg.prog ++ argv

/-- Render the bench block as a terminal-style code block: a synthetic
`$ <command>` prompt line, followed by the literal expected output. -/
private def renderBenchBlock (cfg : BenchConfig) (str : StrLit) : DocElabM Term := do
  let body := s!"{buildPromptLine cfg}\n{str.getString}"
  `(Verso.Doc.Block.code $(quote body))

/-- Render a benchTranscript block as just the literal body (no
synthetic prompt — the body usually already includes one or is verbatim
external output). -/
private def renderTranscriptBlock (str : StrLit) : DocElabM Term := do
  `(Verso.Doc.Block.code $(quote str.getString))

/-! ## The `bench` directive -/

/-- If `path` is absolute, return it; otherwise resolve it relative to
the directive's invocation cwd (i.e. the cwd of the `lake build`
process, typically `docs/`). This is done before applying `cfg.cwd`
so the spawned process can reliably find its binary regardless of
where it ends up running. -/
private def absolutise (path : String) : IO String := do
  if path.startsWith "/" then
    return path
  let here ← IO.currentDir
  return (here / path).toString

/--
Run `cfg.prog` with `cfg.argv` from `cfg.cwd`, capture stdout/stderr/exit,
and `expectString`-diff the stdout against the literal block body using
the configured normalisers.

Throws (logs an error) on any mismatch; returns nothing useful.
-/
private def runAndCheck (cfg : BenchConfig) (str : StrLit) : DocElabM Unit := do
  let normalisers ←
    match parseNormalisers cfg.normalise with
    | .ok ns => pure ns
    | .error e => throwErrorAt str e
  let argv := splitArgv cfg.argv
  let cwd? := if cfg.cwd.isEmpty then none else some cfg.cwd
  let progAbs ← liftM (absolutise cfg.prog)
  let out ← IO.Process.output { cmd := progAbs, args := argv, cwd := cwd? }
  if out.exitCode != cfg.expectExit.toUInt32 then
    throwErrorAt str
      m!"Running `{cfg.prog} {cfg.argv}` (cwd `{cfg.cwd}`) exited {out.exitCode}, expected {cfg.expectExit}\n\
        \nstdout:\n{out.stdout}\n\nstderr:\n{out.stderr}"
  let what := s!"output of `{cfg.prog} {cfg.argv}`"
  let _ ← expectString what str out.stdout
    (preEq := composePreEq normalisers)
    (useLine := composeUseLine normalisers)

@[code_block_expander bench]
def bench : CodeBlockExpander
  | args, str => do
    let cfg ← BenchConfig.parser.run args
    if (← liftM gateEnabled) then
      runAndCheck cfg str
    return #[← renderBenchBlock cfg str]

/-! ## The `benchTranscript` directive -/

structure TranscriptConfig where
  /-- Optional caption-style program label, rendered as the start of the
  block (e.g. `perf stat lake exe …`). Purely cosmetic. -/
  caption : Option String := none

private def TranscriptConfig.parser : ArgParse m TranscriptConfig :=
  TranscriptConfig.mk <$> .named `caption .string true

@[code_block_expander benchTranscript]
def benchTranscript : CodeBlockExpander
  | args, str => do
    let _ ← TranscriptConfig.parser.run args
    return #[← renderTranscriptBlock str]

end BenchDocs.Block
