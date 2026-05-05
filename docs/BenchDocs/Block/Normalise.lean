/-!
Line-shaped normalisers for the `bench` directive. Each normaliser is a
pair of `(preEq, useLine)`: `preEq` rewrites a line before comparison,
`useLine` decides whether a line participates in comparison at all.

Normalisers compose: `preEq` of a list is left-to-right composition;
`useLine` is AND. None of these need a regex library — they hand-roll
small state machines on lists of characters.

Implementations operate on `List Char` rather than `String.Pos` because
Lean's String position API is in flux; converting at the boundary
trades a little throughput for a stable, easy-to-test surface.
-/

namespace BenchDocs.Block

inductive Normaliser where
  /-- Replace numeric+unit timing tokens (`123.456 µs`, `1.23 ms`,
      `987 ns`, `12.5 s`, `2.0 m`, `1 h`) with `<TIME>`. -/
  | timing
  /-- Replace decimal numbers immediately following `C=`, `β=`,
      `cMin=`, `cMax=`, `slope=` with `<NUM>`. Replace `×2^<digits>`
      with `×2^<N>`. -/
  | ratio
  /-- Replace JSON string values for noisy fields (`"hostname"`,
      `"started_at"`, `"lean_version"`, `"platform_target"`,
      `"git_commit"`) with `"<REDACTED>"`. -/
  | envJson
  /-- Skip blank/whitespace-only lines from comparison. -/
  | skipBlank
deriving Repr, BEq, DecidableEq, Inhabited

namespace Normaliser

private def isDigitOrSep (c : Char) : Bool :=
  c.isDigit || c == '.' || c == '_'

/-- Take the longest `List Char` prefix satisfying `p`; return prefix and rest. -/
private def takeWhile (p : Char → Bool) : List Char → List Char × List Char
  | [] => ([], [])
  | c :: rest =>
    if p c then
      let (ts, rs) := takeWhile p rest
      (c :: ts, rs)
    else
      ([], c :: rest)

private def isPrefixOfL : List Char → List Char → Bool
  | [], _ => true
  | _, [] => false
  | a :: as, b :: bs => a == b && isPrefixOfL as bs

private def stripPrefixL : List Char → List Char → Option (List Char)
  | [], rest => some rest
  | _, [] => none
  | a :: as, b :: bs => if a == b then stripPrefixL as bs else none

/-! ## Timing -/

private def timingUnits : List (List Char) :=
  -- Order matters: longer units before shorter ones with the same first char,
  -- so `µs` is preferred over `m`, `ns` over `s`, `ms` over `m`, `us` over a
  -- bare unit (n/a here), etc.
  [['µ','s'], ['n','s'], ['m','s'], ['u','s'], ['s'], ['m'], ['h']]

/-- Try to match a unit at the head of the list; return the rest after the
unit if matched and the character after the unit is end-of-list or
non-letter. Prefers the longest valid match. -/
private def matchUnit (cs : List Char) : Option (List Char) := Id.run do
  let mut best : Option (List Char) := none
  let mut bestLen : Nat := 0
  for u in timingUnits do
    match stripPrefixL u cs with
    | none => continue
    | some after =>
      let nextOK :=
        match after with
        | [] => true
        | c :: _ => !c.isAlpha
      if nextOK && u.length > bestLen then
        best := some after
        bestLen := u.length
  return best

/-- Walk the input; whenever we see `<digit-token> <maybe-spaces> <unit>`
where `<unit>` is followed by EOS or non-letter, emit `<TIME>`. -/
private partial def normaliseTimingL : List Char → List Char
  | [] => []
  | input@(c :: _) =>
    if isDigitOrSep c then
      let (numToken, rest) := takeWhile isDigitOrSep input
      -- A valid number has at least one digit.
      let hasDigit := numToken.any (fun ch => ch.isDigit)
      if !hasDigit then
        c :: normaliseTimingL (input.drop 1)
      else
        let (ws, rest') := takeWhile (· == ' ') rest
        match matchUnit rest' with
        | some after =>
          ('<' :: 'T' :: 'I' :: 'M' :: 'E' :: '>' :: []) ++ normaliseTimingL after
        | none =>
          numToken ++ ws ++ normaliseTimingL rest'
    else
      c :: normaliseTimingL (input.drop 1)

private def normaliseTiming (line : String) : String :=
  String.ofList (normaliseTimingL line.toList)

/-! ## Ratios / verdict numerics -/

private def ratioTags : List (List Char) :=
  -- Same ordering rule as timing units: longer prefixes first.
  [['c','M','i','n','='], ['c','M','a','x','='], ['s','l','o','p','e','='],
   ['β','='], ['b','e','t','a','='], ['C','=']]

/-- After a tag, skip optional spaces and an optional sign, then digits/`.`/`_`.
Returns `(numStart, after)` where `numStart` is the offset of the first
sign/digit char relative to the input, and `after` is the rest after the
number. Returns `none` if no number follows. -/
private def takeTaggedNumber (cs : List Char) : Option (List Char × List Char) := Id.run do
  let (spaces, rest) := takeWhile (· == ' ') cs
  let (sign, rest') :=
    match rest with
    | '-' :: r => (['-'], r)
    | '+' :: r => (['+'], r)
    | _ => ([], rest)
  let (digits, rest'') := takeWhile isDigitOrSep rest'
  if digits.any (fun c => c.isDigit) then
    return some (spaces ++ sign ++ digits, rest'')
  return none

private def numPlaceholder : List Char := "<NUM>".toList
private def repeatMark : List Char := "×2^".toList
private def repeatPlaceholder : List Char := "×2^<N>".toList

private partial def normaliseRatioL : List Char → List Char
  | [] => []
  | input =>
    match firstMatchingTag input ratioTags with
    | some (tag, after) =>
      match takeTaggedNumber after with
      | some (_, rest) => tag ++ numPlaceholder ++ normaliseRatioL rest
      | none => tag ++ normaliseRatioL after
    | none =>
      match stripPrefixL repeatMark input with
      | some after =>
        let (digits, rest) := takeWhile (·.isDigit) after
        if !digits.isEmpty then
          repeatPlaceholder ++ normaliseRatioL rest
        else
          repeatMark ++ normaliseRatioL after
      | none =>
        match input with
        | [] => []
        | c :: rest => c :: normaliseRatioL rest
where
  firstMatchingTag (cs : List Char) : List (List Char) → Option (List Char × List Char)
    | [] => none
    | tag :: tags =>
      match stripPrefixL tag cs with
      | some after => some (tag, after)
      | none => firstMatchingTag cs tags

private def normaliseRatio (line : String) : String :=
  String.ofList (normaliseRatioL line.toList)

/-! ## Env JSON fields -/

private def envFields : List String :=
  ["hostname", "started_at", "lean_version", "platform_target", "git_commit"]

private def doubleQuote : List Char := ['"']

/-- Skip optional whitespace, then `:`, then optional whitespace, then a
`"`. Returns the body of the string (after the opening quote) or `none`
if the structure doesn't match. -/
private def skipToValueBody : List Char → Option (List Char)
  | [] => none
  | ' ' :: rest => skipToValueBody rest
  | ':' :: rest => skipToOpenQuote rest
  | _ => none
where
  skipToOpenQuote : List Char → Option (List Char)
    | [] => none
    | ' ' :: rest => skipToOpenQuote rest
    | '"' :: rest => some rest
    | _ => none

/-- Consume a JSON string body up to (and including) its closing quote.
Returns the rest after the closing quote, or `none` if no closing quote
was found. -/
private partial def skipPastClosingQuote : List Char → Option (List Char)
  | [] => none
  | '\\' :: _ :: rest => skipPastClosingQuote rest
  | '\\' :: [] => none
  | '"' :: rest => some rest
  | _ :: rest => skipPastClosingQuote rest

private def redactedValue : List Char := "\"<REDACTED>\"".toList

/-- Try to find one occurrence of `"<field>"` followed by a string value;
replace the value with `"<REDACTED>"`. Return the rewritten input, or
the original if no occurrence was found. The `Bool` indicates whether a
replacement happened. -/
private partial def replaceOnce (needle : List Char) (input : List Char) : Bool × List Char :=
  go input
where
  go : List Char → Bool × List Char
    | [] => (false, [])
    | input@(c :: rest) =>
      match stripPrefixL needle input with
      | some after =>
        match skipToValueBody after with
        | some bodyRest =>
          match skipPastClosingQuote bodyRest with
          | some afterValue => (true, needle ++ ": ".toList ++ redactedValue ++ afterValue)
          | none =>
            -- malformed; keep original
            let (b, tail) := go rest
            (b, c :: tail)
        | none =>
          let (b, tail) := go rest
          (b, c :: tail)
      | none =>
        let (b, tail) := go rest
        (b, c :: tail)

private partial def replaceAll (needle : List Char) (input : List Char) : List Char :=
  let (replaced, output) := replaceOnce needle input
  -- Terminate even if `replaced` is true, when the rewrite was a fixpoint
  -- (e.g. the value was already `"<REDACTED>"`). Without this guard the
  -- recursion never bottoms out on already-redacted input.
  if replaced && output != input then replaceAll needle output else output

private def normaliseEnvJson (line : String) : String := Id.run do
  let mut chars := line.toList
  for field in envFields do
    let needle := ('"' :: field.toList) ++ ['"']
    chars := replaceAll needle chars
  return String.ofList chars

/-! ## Public API -/

/-- The line transform contributed by this normaliser. -/
def preEq : Normaliser → String → String
  | .timing => normaliseTiming
  | .ratio => normaliseRatio
  | .envJson => normaliseEnvJson
  | .skipBlank => id

private def lineHasNonWhitespace (s : String) : Bool :=
  s.toList.any (fun c => !c.isWhitespace)

/-- The line filter contributed by this normaliser. `true` = participate
in comparison. -/
def useLine : Normaliser → String → Bool
  | .skipBlank => lineHasNonWhitespace
  | _ => fun _ => true

end Normaliser

/-- Compose a list of normalisers into a single line transform
(left-to-right). -/
def composePreEq (ns : Array Normaliser) (s : String) : String :=
  ns.foldl (init := s) fun acc n => n.preEq acc

/-- AND-compose a list of normalisers into a single line filter. -/
def composeUseLine (ns : Array Normaliser) (s : String) : Bool :=
  ns.all (fun n => n.useLine s)

end BenchDocs.Block
