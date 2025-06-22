namespace Runner


structure Solver where
  year : Nat
  day : Nat
  run : IO Unit


def solver [ToString α] [ToString β]
    (year : Nat) (day : Nat)
    (part₁ : String → α) (part₂ : String → β) :
    Solver :=
  {year, day, run}
  where
    run := do
      let path := s!"../../data/{year}/{pad day}.txt"
      let input ← IO.FS.readFile path
      IO.println s!"{year}.{pad day}"
      let result₁ := part₁ input
      IO.println s!"1️⃣ {result₁}"
      let result₂ := part₂ input
      IO.println s!"2️⃣ {result₂}"
    pad d := if d < 10 then s!"0{d}" else s!"{d}"


def findSolver (args : List String) (solvers : List Solver) : Except String Solver := do
  let [year, day] := args
    | throw "invalid number of arguments"
  let some year := year.toNat?
    | throw "invalid year"
  let some day := day.toNat?
    | throw "invalid day"
  let some solver := solvers.find? (fun s => s.year == year && s.day == day)
    | throw "no solver found"
  pure solver


def process (args : List String) (solvers : List Solver) : IO Unit := do
  match findSolver args solvers with
  | .ok solver => solver.run
  | .error err => IO.println s!"❌ {err}"


end Runner


export Runner (Solver solver process)
