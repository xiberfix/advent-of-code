import AdventOfCode.Parser
import AdventOfCode.Utils

open Std

namespace Year2024.Day02

def parser := linesOf (int.sepBy1 ' ')


def isSafeStep (step : Int) : Bool := 1 ≤ step && step ≤ 3

partial def isSafeAscending (levels : Array Int) (tolerance : Nat) : Bool :=
  let steps := levels.iter.zipNext.map (fun (curr, next) => next - curr)
  let notSafeIdx := steps.findIdx? (not ∘ isSafeStep)
  match notSafeIdx with
  | none => true
  | some idx =>
    if tolerance > 0 then
      isSafeAscending (levels.eraseIdx! idx) (tolerance - 1) ||
      isSafeAscending (levels.eraseIdx! (idx + 1)) (tolerance - 1)
    else
      false

def isSafe (report : Array Int) (tolerance : Nat) : Bool :=
  isSafeAscending report tolerance ||
  isSafeAscending report.reverse tolerance


def part₁ (input : String) : Int :=
  let reports := input.parse parser
  reports.countP (fun report => isSafe report 0)


def part₂ (input : String) : Int :=
  let reports := input.parse parser
  reports.countP (fun report => isSafe report 1)


def test : String := r"
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"

#eval part₁ test -- 2
#eval part₂ test -- 4
