namespace Year2024.Day01


def parse (input : String) : List (Int × Int) :=
  input.trim.splitOn "\n"
    |>.map (·.splitOn "   ")
    |>.map (fun pair => ⟨pair[0]!.toInt!, pair[1]!.toInt!⟩)


def part₁ (input : String) : Int :=
  let (ls, rs) := parse input |>.unzip
  List.zipWith dist ls.mergeSort rs.mergeSort |>.sum
  where
    dist a b := (a - b).natAbs


def part₂ (input : String) : Int :=
  0


def test : String := r"
3   4
4   3
2   5
1   3
3   9
3   3
".trimLeft

#eval part₁ test -- 11
#eval part₂ test -- 0
