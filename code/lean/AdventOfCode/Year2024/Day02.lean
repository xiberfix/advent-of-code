namespace Year2024.Day02


def parse (input : String) : List (List Int) :=
  input.trim
    |>.splitOn "\n"
    |>.map (fun line => line |>.splitOn " " |>.map (·.toInt!))


def part₁ (input : String) : Int :=
  parse input
    |>.filter check
    |>.length
  where
    check report :=
      let deltas := List.zipWith (· - ·) report (report.tail)
      let dec := deltas.all (fun d => 1 <= d && d <= 3)
      let inc := deltas.all (fun d => -3 <= d && d <= -1)
      dec || inc


def part₂ (input : String) : Int :=
  0


def test : String := r"
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
".trimLeft

#eval part₁ test -- 2
#eval part₂ test -- 0
