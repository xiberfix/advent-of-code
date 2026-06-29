import AdventOfCode.Parser
import AdventOfCode.Utils

namespace Year2024.Day03

inductive Op where
  | mul (a b : Int)
  | on
  | off
deriving Inhabited, Repr, BEq

def parser := many (step op) <&> (·.filterMap id)
  where
    op : Parser Op := choice [
      p!"mul({nat},{nat})" <&> fun (a, b) => .mul a b,
      p!"do()"             <&> fun _      => .on,
      p!"don't()"          <&> fun _      => .off,
    ]
    step {α : Type} (p : Parser α) : Parser (Option α) := choice [
      p       <&> fun x => some x,
      anyChar <&> fun _ => none
    ]


def part₁ (input : String) : Int :=
  let ops := input.parse parser
  Id.run do
    let mut total := 0
    for op in ops do
      match op with
      | .mul a b => total := total + a * b
      | _        => ()
    total


def part₂ (input : String) : Int :=
  let ops := input.parse parser
  Id.run do
    let mut total := 0
    let mut act := true
    for op in ops do
      match op with
      | .mul a b => total := total + if act then a * b else 0
      | .on      => act := true
      | .off     => act := false
    total


def test₁ : String := r"
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
"

def test₂ : String := r"
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
"

#eval part₁ test₁ -- 161
#eval part₂ test₂ -- 48
