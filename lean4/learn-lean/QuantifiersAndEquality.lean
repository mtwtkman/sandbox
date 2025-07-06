example (α : Type) (p q : α → Prop) :
    (∀ x : α, p x ∧ q x) → ∀ y : α, p y :=
  fun h : ∀ x : α, p x ∧ q x =>
    fun y : α =>
      show p y from (h y).left

variable (α : Type) (r : α → α → Prop)
variable (trans_r : ∀ x y z, r x y -> r y z -> r x z)
variable (a b c : α)
variable (hab : r a b) (hbc : r b c)

#check trans_r
#check trans_r a b c
#check trans_r a b c hab
#check trans_r a b c hab hbc

variable (trans_r : ∀ {x y z}, r x y -> r y z -> r x z)

#check trans_r
#check trans_r hab
#check trans_r hab hbc

variable (refl_r : ∀ x, r x x)
variable (symm_r : ∀ {x y}, r x y -> r y x)

example (a b c d : α) (hab : r a b) (hcb : r c b) (hcd : r c d) : r a d :=
  trans_r (trans_r hab (symm_r hcb)) hcd

#check Eq.refl
#check Eq.symm
#check Eq.trans

universe u
#check @Eq.refl.{u}
#check @Eq.symm.{u}
#check @Eq.trans.{u}

variable (a b c d : α)
variable (hab : a = b) (hcb : c = b) (hcd : c = d)

example : a = d :=
  Eq.trans (Eq.trans hab (Eq.symm hcb)) hcd

variable (β : Type)

example (f : α → β) (a : α) : (fun x => f x) a = f a := Eq.refl _
example (a : α) (b : β) : (a, b).1 = a := Eq.refl _
example : 2 + 3 = 5 := Eq.refl _

example (f : α → β) (a : α) : (fun x => f x) a = f a := rfl
example (a : α) ( b : β) : (a, b).1 = a := rfl
example : 2 + 3 = 5 := rfl

example (α : Type) (a b : α) (p : α → Prop)
        (h1 : a = b) (h2 : p a) : p b :=
    Eq.subst h1 h2
example (α : Type) (a b : α) (p : α → Prop)
        (h1 : a = b) (h2 : p a) : p b :=
    h1 ▸ h2

variable (f g : α → Nat)
variable (h₁ : a = b)
variable (h₂ : f = g)

example : f a = f b := congrArg f h₁
example : f a = g a := congrFun h₂ a
example : f a = g b := congr h₂ h₁

variable (a b c : Nat)
example : a + 0 = a := Nat.add_zero a
example : 0 + a = a := Nat.zero_add a
example : a * 1 = a := Nat.mul_one a
example : 1 * a = a := Nat.one_mul a
example : a + b = b + a := Nat.add_comm a b
example : a + b + c = a + (b + c) := Nat.add_assoc a b c
example : a * b = b * a := Nat.mul_comm a b
example : a * b * c = a * (b * c) := Nat.mul_assoc a b c
example : a * (b + c) = a * b + a * c := Nat.mul_add a b c
example : a * (b + c) = a * b + a * c := Nat.left_distrib a b c
example : (a + b) * c = a * c + b * c := Nat.add_mul a b c
example : (a + b) * c = a * c + b * c := Nat.right_distrib a b c
example (x y : Nat) :
    (x + y) * (x + y) =
    x * x + y * x + x * y + y * y :=
  have h₁ : (x + y) * (x + y) = (x + y) * x + (x + y) * y :=
    Nat.mul_add (x + y) x y
  have h₂ : (x + y) * (x +y) = x * x + y * x + (x * y + y * y) :=
    (Nat.add_mul x y x) ▸ (Nat.add_mul x y y) ▸ h₁
  h₂.trans (Nat.add_assoc (x * x + y * x) (x * y) (y * y)).symm
