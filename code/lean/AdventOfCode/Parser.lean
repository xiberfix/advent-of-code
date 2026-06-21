import Lean


def Parser (α : Type) := String.Slice → Option (α × String.Slice)


namespace Parser

-- TODO: error message
-- TODO: position tracking

def run (p : Parser α) (s : String) : Option α :=
  match p s with
  | none => none
  | some (a, _) => some a

def run! [Inhabited α] (p : Parser α) (s : String) : α :=
  p.run s |>.get!


instance : Functor Parser where
  map f p := fun s =>
    match p s with
    | none => none
    | some (a, s') => some (f a, s')

instance : Monad Parser where
  pure a := fun s =>
    some (a, s)
  bind p f := fun s =>
    match p s with
    | none => none
    | some (a, s') => f a s'

instance : Alternative Parser where
  failure := fun _ =>
    none
  orElse p q := fun s =>
    match p s with
    | none => q () s
    | some (a, s') => some (a, s')


def next? (s : String.Slice) : Option (Char × String.Slice) := do
  let c ← s.front?
  return ⟨c, s.drop 1⟩

def satisfy (p : Char → Bool) : Parser Char := fun s =>
  match next? s with
  | none => none
  | some (c, s') => if p c then some (c, s') else none


def char (lit : Char) : Parser Char :=
  satisfy (· == lit)

def string (lit : String) : Parser String.Slice := fun s =>
  match s.dropPrefix? lit with
  | none => none
  | some s' => some (lit, s')

def eos : Parser Unit := fun s =>
  if s.isEmpty then some ((), s) else none

def eol : Parser Unit :=
  char '\n' *> pure ()

-- TODO: implement `span` as `takeWhile` and `dropWhile`

def takeChars1 (p : Char → Bool) : Parser String.Slice := fun s =>
  let r := s.takeWhile p
  let s' := s.dropWhile p
  if r.isEmpty then none else some (r, s')

def dropChars1 (p : Char → Bool) : Parser Unit := fun s =>
  let r := s.takeWhile p
  let s' := s.dropWhile p
  if r.isEmpty then none else some ((), s')


def nat : Parser Nat := do
  let ds ← takeChars1 Char.isDigit
  let n := ds.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
  return n

def int : Parser Int := do
  let negative ← (char '-' *> pure true) <|> (char '+' *> pure false) <|> pure false
  let n ← nat
  let i := if negative then Int.negOfNat n else Int.ofNat n
  return i


def optional (p : Parser α) : Parser (Option α) :=
  (some <$> p) <|> pure none

def choice (ps : List (Parser α)) : Parser α :=
  ps.foldr (· <|> ·) failure


partial def foldl (p : Parser α) (f : β → α → β) (init : β) : Parser β :=
  let rec loop (acc : β) := do
    match ← optional p with
    | none => pure acc
    | some a => loop (f acc a)
  loop init

def orEmpty (p : Parser (Array α)) : Parser (Array α) :=
  p <|> pure #[]

def array (init step : Parser α) : Parser (Array α) := do
  foldl step Array.push #[←init]


class ToSep (σ : Type) where
  toSep : σ → Parser Unit

export ToSep (toSep)

instance : ToSep (Parser α) where
  toSep p := p *> pure ()
instance : ToSep Char where
  toSep c := char c *> pure ()
instance : ToSep String where
  toSep s := string s *> pure ()


def many1 (p : Parser α) : Parser (Array α) := array p p
def many (p : Parser α) : Parser (Array α) := orEmpty (many1 p)

def sepBy1 [ToSep σ] (p : Parser α) (sep : σ) : Parser (Array α) := array p (toSep sep *> p)
def sepBy [ToSep σ] (p : Parser α) (sep : σ) : Parser (Array α) := orEmpty (sepBy1 p sep)

def endBy1 [ToSep σ] (p : Parser α) (sep : σ) : Parser (Array α) := array (p <* toSep sep) (p <* toSep sep)
def endBy [ToSep σ] (p : Parser α) (sep : σ) : Parser (Array α) := orEmpty (endBy1 p sep)


def linesOf (p : Parser α) : Parser (Array α) :=
  sepBy1 p eol

def blocksOf (p : Parser α) : Parser (Array α) :=
  sepBy1 p (eol *> eol)


namespace Template

-- template parser
-- p!"v={int},{int}" : Parser (Int × Int)

open Lean Elab Term Macro

private def trim (lit : String) (first last : Bool) : String :=
  let lit := if first then lit.dropWhile '\n' else lit
  let lit := if last then lit.dropEndWhile '\n' else lit
  lit.toString

private def fixed (lit : String) : Parser Unit := fun s =>
  match s.dropPrefix? lit with
  | none => none
  | some s' => some ((), s')

private def combine (ts : Array (TSyntax `term)) : MacroM (TSyntax `term) :=
  match ts with
  | #[]  => `(())
  | #[x] => `($x)
  | _    => `(($(ts[0]!),$(ts[1:]),*))

private def expand (template : TSyntax `interpolatedStrKind) : MacroM (TSyntax `term) := do
  let mut statements : Array (TSyntax `doElem) := #[]
  let mut captures : Array (TSyntax `ident) := #[]
  let args := template.raw.getArgs
  for (arg, i) in args.zipIdx do
    if let some lit := arg.isInterpolatedStrLit? then
      let lit := trim lit (i == 0) (i == args.size - 1)
      unless lit.isEmpty do
        let statement ← `(doElem| fixed $(quote lit))
        statements := statements.push statement
    else
      let capture ← mkFreshIdent arg
      captures := captures.push capture
      let statement ← `(doElem| let $capture:ident ← $(⟨arg⟩):term)
      statements := statements.push statement
  let result ← `(doElem| pure $(← combine captures))
  statements := statements.push result
  `(do $[$statements:doElem]*)


syntax:max (name := pTemplate) "p!" interpolatedStr(term) : term

macro_rules
  | `(p!$s:interpolatedStr) => expand s

end Template


end Parser


export Parser (
  satisfy char string eos eol nat int
  many1 many sepBy1 sepBy endBy1 endBy linesOf blocksOf
)


def String.parse [Inhabited α] (input : String) (p : Parser α) : α :=
  (many eol *> p <* many eol <* eos).run! input
