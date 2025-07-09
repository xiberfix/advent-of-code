import Batteries.Data.List
import AdventOfCode.Utils

namespace Year2024.Day04


structure Grid (α : Type u) where
  data : Array α
  size : Vec₂ Nat
deriving Inhabited, Repr

def Grid.get? (g : Grid α) (pos : Vec₂ Int) : Option α := do
  let x ← pos.x.toNat?
  let y ← pos.y.toNat?
  guard (x < g.size.x)
  guard (y < g.size.y)
  let index := g.size.x * y + x
  g.data[index]?

def Grid.get! [Inhabited α] (g : Grid α) (pos : Vec₂ Int) : α :=
  g.get? pos |>.get!

def Grid.getD (g : Grid α) (pos : Vec₂ Int) (default : α) : α :=
  g.get? pos |>.getD default


def Grid.parse! (s : String) : Grid Char :=
  let lines := s.trim.splitOn "\n"
  let data := lines |>.flatMap (·.toList) |>.toArray
  let size := ⟨lines[0]!.length, lines.length⟩
  { data, size }

instance : ToString (Grid Char) where
  toString g :=
    let lines := g.data.toList.toChunks g.size.x |>.map String.mk
    lines.intersperse "\n" |> String.join


def parse (input : String) : Grid Char :=
  Grid.parse! input


abbrev V := Vec₂ Int

def directions : List V := [
  ⟨-1, -1⟩, ⟨ 0, -1⟩, ⟨ 1, -1⟩,
  ⟨-1,  0⟩,           ⟨ 1,  0⟩,
  ⟨-1,  1⟩, ⟨ 0,  1⟩, ⟨ 1,  1⟩,
]

def Grid.positions (g : Grid α) : List V :=
  let xs := List.range g.size.x |>.map Int.ofNat
  let ys := List.range g.size.y |>.map Int.ofNat
  List.product xs ys |>.map (fun (x, y) => ⟨x, y⟩)

def expand (pos dir : V) (n : Nat) : List V :=
  List.range n |>.map Int.ofNat |>.map (pos + dir * .)

def part₁ (input : String) : Int :=
  let g := parse input
  let pattern := "XMAS".toList
  let k := pattern.length
  List.product g.positions directions
    |>.map (fun (pos, dir) => expand pos dir k)
    |>.map (fun ray => ray.map (g.getD · '.'))
    |>.filter (. == pattern)
    |>.length


def part₂ (input : String) : Int :=
  0


def test : String := r"
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
".trimLeft

#eval part₁ test -- 18
#eval part₂ test -- 0
