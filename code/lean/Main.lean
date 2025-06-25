import AdventOfCode


def main (args : List String) : IO Unit := do
  process args solvers
  where
    solvers := Year2024.solvers
