namespace Year2024.Day02


def parse (input : String) : List (List Int) :=
  input.trim.splitOn "\n"
    |>.map (·.splitOn " ")
    |>.map (· |>.map (·.toInt!))


def check₁ (report : List Int) : Bool := check report || check report.reverse
  where
    check levels := List.zipWith (· - ·) levels (levels.tail) |>.all safe
    safe d := d == 1 || d == 2 || d == 3

def part₁ (input : String) : Int :=
  parse input
    |>.filter check₁
    |>.length


def slices (xs : List α) : List (List α) :=
  match xs with
  | [] => []
  | x :: xs => xs :: (slices xs).map (x :: ·)

def check₂ (report : List Int) : Bool := (slices report).any check₁

def part₂ (input : String) : Int :=
  parse input
    |>.filter check₂
    |>.length


def test : String := r"
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
".trimLeft

#eval part₁ test -- 2
#eval part₂ test -- 4
