import AdventOfCode.Parser
import AdventOfCode.Utils

namespace Year2024.Day01

def parser := linesOf p!"{int}   {int}"


def part₁ (input : String) : Int :=
  let (ls, rs) := input.parse parser |>.unzip
  let dist := fun (x, y) => (x - y).natAbs
  ls.qsort.iter.zip rs.qsort.iter |>.map dist |>.sum


def part₂ (input : String) : Int :=
  let (ls, rs) := input.parse parser |>.unzip
  let counts := rs.iter.counts
  ls.iter |>.map (fun k => k * counts.getD k 0) |>.sum


def test : String := r"
3   4
4   3
2   5
1   3
3   9
3   3
"

#eval part₁ test -- 11
#eval part₂ test -- 31
