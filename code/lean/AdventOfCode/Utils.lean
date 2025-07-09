structure Vec₂ (α : Type u) where
  x : α
  y : α
deriving Inhabited, Repr, BEq, Hashable

instance [Add α] : Add (Vec₂ α) where
  add u v := ⟨u.x + v.x, u.y + v.y⟩

instance [Sub α] : Sub (Vec₂ α) where
  sub u v := ⟨u.x - v.x, u.y - v.y⟩

instance [Mul α] : HMul α (Vec₂ α) (Vec₂ α) where
  hMul a u := ⟨a * u.x, a * u.y⟩

instance [Mul α] : HMul (Vec₂ α) α (Vec₂ α) where
  hMul u a := ⟨u.x * a, u.y * a⟩

instance [Neg α] : Neg (Vec₂ α) where
  neg u := ⟨-u.x, -u.y⟩

instance [ToString α] : ToString (Vec₂ α) where
  toString u := s!"({u.x}, {u.y})"

instance [Coe α β] : Coe (Vec₂ α) (Vec₂ β) where
  coe u := ⟨u.x, u.y⟩
