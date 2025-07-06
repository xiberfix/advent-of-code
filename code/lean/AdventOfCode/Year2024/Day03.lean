import Regex

namespace Year2024.Day03


inductive Op where
  | mul (a b : Int)
  | enable
  | disable
deriving Inhabited

def parse (input : String) : Array Op :=
  re! r"mul\((\d+),(\d+)\)|do\(\)|don't\(\)"
    |>.captureAll input
    |>.map (·.toArray.map (·.map (·.toString)))
    |>.map (parseOp ·)
  where
    parseOp
      | #[some "do()",    none,   none  ] => .enable
      | #[some "don't()", none,   none  ] => .disable
      | #[some _,         some a, some b] => .mul a.toNat! b.toNat!
      | _ => panic! "unreachable"


def part₁ (input : String) : Int :=
  let ops := parse input
  Id.run do
    let mut res := 0
    for op in ops do
      match op with
      | .mul a b => res := res + a * b
      | _ => ()
    res


def part₂ (input : String) : Int :=
  let ops := parse input
  Id.run do
    let mut res := 0
    let mut flag := true
    for op in ops do
      match op with
      | .mul a b => if flag then res := res + a * b
      | .enable  => flag := true
      | .disable => flag := false
    res


def test₁ : String := r"
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
".trimLeft

def test₂ : String := r"
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
".trimLeft

#eval part₁ test₁ -- 161
#eval part₂ test₂ -- 48
