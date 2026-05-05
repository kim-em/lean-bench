import Lean.Elab.Command
import Verso
import Verso.Doc.ArgParse
import Verso.Doc.Elab.Monad
import VersoManual
import BenchDocs.Block.Bench
import BenchDocs.Block.ExpectString
import BenchDocs.Block.Normalise

/-!
Multi-block harness for documenting downstream consumer projects.

A `::: tempProj` directive wraps a series of inner code blocks that
collectively describe a complete mini Lean project (a `lakefile.toml`,
one or more `.lean` files, and a final command + expected output). At
directive close, the harness materialises the project in a temp dir
(with `{{LEAN_BENCH_PATH}}` resolved to the absolute path of the
lean-bench root), runs `lake build`, runs the named CLI invocation,
and diffs stdout against the expected literal via `expectString`.

Live execution is gated on `LEAN_BENCH_DOCS_CHECK` exactly like the
plain `bench` directive, so editor elaboration is fast.

Modeled on `verso-manual/VersoManual/InlineLean/IO.lean`.
-/

open Lean Elab
open Verso ArgParse Doc Elab Genre.Manual

namespace BenchDocs.Block

variable {m : Type → Type}
variable [Monad m] [MonadInfoTree m] [MonadLiftT CoreM m] [MonadEnv m] [MonadError m]

/-! ## Shared state -/

/-- A single declared file in a temp project. -/
structure TempProjFile where
  path : String
  contents : String
deriving Inhabited

/-- One commanded run-and-check pair: argv (after `lake`) plus the
expected stdout literal and any normalisers. -/
structure TempProjRun where
  args : Array String
  expected : StrLit
  normalise : Array Normaliser
  expectExit : Nat := 0

/-- Full state collected from inner blocks during a `::: tempProj`
directive expansion. -/
structure TempProjState where
  files : Array TempProjFile := #[]
  runs  : Array TempProjRun := #[]
  active : Bool := false
deriving Inhabited

initialize tempProjStateExt : EnvExtension TempProjState ←
  registerEnvExtension (pure {})

private def getState : CoreM TempProjState := do
  return tempProjStateExt.getState (← getEnv)

private def modifyState (f : TempProjState → TempProjState) : CoreM Unit := do
  modifyEnv (tempProjStateExt.modifyState · f)

private def startTempProj : CoreM Unit := do
  let st ← getState
  if st.active then
    throwError "Nested `::: tempProj` directives are not supported"
  modifyState fun _ => { active := true }

private def resetTempProj : CoreM Unit :=
  modifyState fun _ => {}

private def addFile (file : TempProjFile) : CoreM Unit := do
  let st ← getState
  unless st.active do
    throwError "tempProjFile blocks must appear inside a `::: tempProj` directive"
  modifyState fun s => { s with files := s.files.push file }

private def addRun (run : TempProjRun) : CoreM Unit := do
  let st ← getState
  unless st.active do
    throwError "tempProjRun blocks must appear inside a `::: tempProj` directive"
  modifyState fun s => { s with runs := s.runs.push run }

/-! ## Path substitution -/

/-- Replace every occurrence of `{{LEAN_BENCH_PATH}}` in a file body
with the absolute path to the lean-bench repository root.

The directive runs from within the docs/ subpackage, so the repo root
is one level up from `IO.currentDir`. -/
private def leanBenchAbsRoot : IO String := do
  let here ← IO.currentDir
  let parent := here.parent.getD here
  return parent.toString

private def substitute (s : String) (token : String) (replacement : String) : String :=
  s.replace token replacement

/-! ## Inner code-block directives -/

structure FileConfig where
  path : String
deriving Inhabited

private def FileConfig.parser : ArgParse m FileConfig :=
  FileConfig.mk <$> .named `path .string false

instance : FromArgs FileConfig m := ⟨FileConfig.parser⟩

/-- A code block inside `::: tempProj` that declares one file of the
mini-project. The block body is the file's content. The path is
relative to the project root. -/
@[code_block_expander tempProjFile]
def tempProjFile : CodeBlockExpander
  | args, str => do
    let cfg ← FileConfig.parser.run args
    addFile { path := cfg.path, contents := str.getString }
    return #[← `(Verso.Doc.Block.code $(quote str.getString))]

structure RunConfig where
  /-- Argv passed to `lake` (e.g. `"exe bench list"`). -/
  args : String
  /-- Comma-separated list of normaliser names. -/
  normalise : String := ""
  /-- Expected exit code; default 0. -/
  expectExit : Nat := 0

private def RunConfig.parser : ArgParse m RunConfig :=
  RunConfig.mk
    <$> .named  `args       .string false
    <*> .namedD `normalise  .string ""
    <*> .namedD `expectExit .nat    0

instance : FromArgs RunConfig m := ⟨RunConfig.parser⟩

private def parseNormalisersArg (s : String) : Except String (Array Normaliser) := Id.run do
  if !s.toList.any (fun c => !c.isWhitespace) then return .ok #[]
  let parts := s.splitOn "," |>.map (fun p =>
    String.ofList (p.toList.filter (fun c => !c.isWhitespace)))
  let mut out : Array Normaliser := #[]
  for p in parts do
    if p.isEmpty then continue
    match p with
    | "timing" => out := out.push .timing
    | "ratio"  => out := out.push .ratio
    | "envJson" => out := out.push .envJson
    | "skipBlank" => out := out.push .skipBlank
    | other => return .error s!"Unknown normaliser '{other}'"
  return .ok out

/-- A code block inside `::: tempProj` that declares one CLI invocation
and its expected stdout. The block body is the expected output. -/
@[code_block_expander tempProjRun]
def tempProjRun : CodeBlockExpander
  | args, str => do
    let cfg ← RunConfig.parser.run args
    let normalisers ←
      match parseNormalisersArg cfg.normalise with
      | .ok ns => pure ns
      | .error e => throwErrorAt str e
    let argv := splitArgv cfg.args
    addRun {
      args := argv
      expected := str
      normalise := normalisers
      expectExit := cfg.expectExit
    }
    return #[← `(Verso.Doc.Block.code $(quote str.getString))]

/-! ## The outer directive -/

structure TempProjConfig where
  /-- Free-form tag used in error messages. -/
  tag : String

private def TempProjConfig.parser : ArgParse m TempProjConfig :=
  TempProjConfig.mk <$> .named `tag .string false

instance : FromArgs TempProjConfig m := ⟨TempProjConfig.parser⟩

private def gateEnabled : IO Bool := do
  return (← IO.getEnv "LEAN_BENCH_DOCS_CHECK").isSome

private def materialiseAndCheck (cfg : TempProjConfig) (firstStr : StrLit) : DocElabM Unit := do
  let st ← liftM getState
  if st.files.isEmpty then
    throwErrorAt firstStr s!"::: tempProj ({cfg.tag}) has no `tempProjFile` blocks"
  if st.runs.isEmpty then
    throwErrorAt firstStr s!"::: tempProj ({cfg.tag}) has no `tempProjRun` blocks"
  let absRoot ← liftM leanBenchAbsRoot
  IO.FS.withTempDir fun dir => do
    -- Write the toolchain so `lake build` picks the same elan toolchain
    -- as the docs subpackage (which already matches lean-bench).
    let toolchain ← IO.FS.readFile "lean-toolchain"
    IO.FS.writeFile (dir / "lean-toolchain") toolchain
    -- Write user files, substituting the lean-bench abs-path token.
    for f in st.files do
      let abs := dir / f.path
      if let some parent := abs.parent then
        IO.FS.createDirAll parent
      let body := substitute f.contents "{{LEAN_BENCH_PATH}}" absRoot
      IO.FS.writeFile abs body
    -- Build once.
    let build ← IO.Process.output { cmd := "lake", args := #["build"], cwd := some dir.toString }
    if build.exitCode != 0 then
      throwErrorAt firstStr m!"\
        \n[tempProj {cfg.tag}] `lake build` failed in {dir.toString} (exit {build.exitCode})\n\
        stdout:\n{build.stdout}\n\nstderr:\n{build.stderr}"
    -- Run each declared command, diffing stdout via expectString.
    for run in st.runs do
      let argv := #["exe"] ++ run.args
      let out ← IO.Process.output { cmd := "lake", args := argv, cwd := some dir.toString }
      if out.exitCode != run.expectExit.toUInt32 then
        throwErrorAt run.expected m!"\
          \n[tempProj {cfg.tag}] `lake {String.intercalate " " argv.toList}` exited {out.exitCode}, expected {run.expectExit}\n\
          stdout:\n{out.stdout}\n\nstderr:\n{out.stderr}"
      let what := s!"output of `lake {String.intercalate " " argv.toList}`"
      let _ ← expectString what run.expected out.stdout
        (preEq := composePreEq run.normalise)
        (useLine := composeUseLine run.normalise)

@[directive_expander tempProj]
def tempProj : DirectiveExpander
  | args, blocks => do
    let cfg ← TempProjConfig.parser.run args
    liftM <| (startTempProj : CoreM Unit)
    let body ← blocks.mapM elabBlock
    -- After inner blocks have populated state, materialise & check.
    -- Use the first inner block's syntax for diagnostic positioning if
    -- something fails before any tempProjRun was reached.
    let firstStrPos : StrLit := ⟨Syntax.missing⟩
    if (← liftM (gateEnabled : IO Bool)) then
      try
        materialiseAndCheck cfg firstStrPos
      finally
        liftM <| (resetTempProj : CoreM Unit)
    else
      liftM <| (resetTempProj : CoreM Unit)
    return #[← `(Verso.Doc.Block.concat #[$[$body],*])]

end BenchDocs.Block
