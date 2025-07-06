import Regex

namespace Year2024.Day03


def parse (input : String) : Array (Nat × Nat) :=
  re! r"mul\((\d+),(\d+)\)"
    |>.captureAll input
    |>.map (·.toArray)
    |>.map (fun groups => ⟨groups[1]!.get!.toNat?.get!, groups[2]!.get!.toNat?.get!⟩)


def part₁ (input : String) : Int :=
  parse input
    |>.map (fun (a, b) => a * b)
    |>.sum


def part₂ (input : String) : Int :=
  0


def test : String := r"
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
".trimLeft

#eval part₁ test -- 161
#eval part₂ test -- 0
