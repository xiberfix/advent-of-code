namespace Parser


def Parser (α : Type) := String.Slice → Option (α × String.Slice)


-- TODO: error message
-- TODO: position tracking
-- TODO: treat no `eos` as error

def Parser.run (p : Parser α) (s : String) : Option α :=
  match p s with
  | none => none
  | some (a, _) => some a

def Parser.run! [Inhabited α] (p : Parser α) (s : String) : α :=
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

-- TODO: return `Unit` for `char` and `string`

def char (lit : Char) : Parser Char :=
  satisfy (· == lit)

def string (lit : String) : Parser String.Slice := fun s =>
  match s.dropPrefix? lit with
  | none => none
  | some s' => some (lit, s')

def eos : Parser Unit := fun s =>
  if s.isEmpty then some ((), s) else none

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

def many1 (p : Parser α) : Parser (Array α) := array p p
def many (p : Parser α) : Parser (Array α) := orEmpty (many1 p)

def sepBy1 (p : Parser α) (sep : Parser β) : Parser (Array α) := array p (sep *> p)
def sepBy (p : Parser α) (sep : Parser β) : Parser (Array α) := orEmpty (sepBy1 p sep)

def endBy1 (p : Parser α) (sep : Parser β) : Parser (Array α) := array (p <* sep) (p <* sep)
def endBy (p : Parser α) (sep : Parser β) : Parser (Array α) := orEmpty (endBy1 p sep)
