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

variable (a b c d e : Nat)

theorem T
    (h1 : a = b)
    (h2 : b = c + 1)
    (h3 : c = d)
    (h4 : e = 1 + d) :
    a = e :=
  calc
    a = b := h1
    _ = c + 1 := h2
    _ = d + 1 := congrArg Nat.succ h3
    _ = 1 + d := Nat.add_comm d 1
    _ = e := Eq.symm h4

theorem T2
    (h1 : a = b)
    (h2 : b = c + 1)
    (h3 : c = d)
    (h4 : e = 1 + d) :
    a = e :=
  calc
    a = b := by rw [h1]
    _ = c + 1 := by rw [h2]
    _ = d + 1 := by rw [h3]
    _ = 1 + d := by rw [Nat.add_comm]
    _ = e := by rw [h4]

theorem T3
    (h1 : a = b)
    (h2 : b = c + 1)
    (h3 : c = d)
    (h4 : e = 1 + d) :
    a = e :=
  calc
    a = d + 1 := by rw [h1, h2, h3]
    _ = 1 + d := by rw [Nat.add_comm]
    _ = e := by rw [h4]

theorem T4
    (h1 : a = b)
    (h2 : b = c + 1)
    (h3 : c = d)
    (h4 : e = 1 + d) :
    a = e :=
  by rw [h1, h2, h3, Nat.add_comm, h4]

theorem T5
    (h1 : a = b)
    (h2 : b = c + 1)
    (h3 : c = d)
    (h4 : e = 1 + d) :
    a = e :=
  by simp [h1, h2, h3, Nat.add_comm, h4]

variable (a b c d : Nat)
example (h1 : a = b) (h2 : b ≤ c) (h3 : c + 1 < d) : a < d :=
  calc
    a = b := h1
    _ < b + 1 := Nat.lt_succ_self b
    _ ≤ c + 1 := Nat.succ_le_succ h2
    _ < d := h3

def divides (x y : Nat) : Prop :=
  ∃ k, k*x = y

def divides_trans (h₁ : divides x y) (h₂ : divides y z) : divides x z :=
  let ⟨k₁, d₁⟩ := h₁
  let ⟨k₂, d₂⟩ := h₂
  ⟨k₁ * k₂, by rw [Nat.mul_comm k₁ k₂, Nat.mul_assoc, d₁, d₂]⟩

def divides_mul (x : Nat) (k : Nat) : divides x (k*x) :=
  ⟨k, rfl⟩

instance : Trans divides divides divides where
  trans := divides_trans

example (h₁ : divides x y) (h₂ : y = z) : divides x (2*z) :=
  calc
    divides x y := h₁
    _ = z := h₂
    divides _ (2*z) := divides_mul ..

infix:50 " | " => divides

example (h₁ : divides x y) (h₂ : y = z) : divides x (2*z) :=
  calc
    x | y := h₁
    _ = z := h₂
    _ | 2*z := divides_mul ..

variable (x y : Nat)
example : (x + y) * (x + y) = x * x + y * x + x * y + y * y :=
  calc
    (x + y) * (x + y) = (x + y) * x + (x + y) * y :=
      by rw [Nat.mul_add]
    _ = x * x + y * x + (x + y) * y :=
      by rw [Nat.add_mul]
    _ = x * x + y * x + (x * y + y * y) :=
      by rw [Nat.add_mul]
    _ = x * x + y * x + x * y + y * y :=
      by rw [←Nat.add_assoc]

example : (x + y) * (x + y) = x * x + y * x + x * y + y * y :=
  calc (x + y) * (x + y)
    _ = (x + y) * x + (x + y) * y :=
      by rw [Nat.mul_add]
    _ = x * x + y * x + (x + y) * y :=
      by rw [Nat.add_mul]
    _ = x * x + y * x + (x * y + y * y) :=
      by rw [Nat.add_mul]
    _ = x * x + y * x + x * y + y * y :=
      by rw [←Nat.add_assoc]

example : (x + y) * (x + y) = x * x + y * x + x * y + y * y := by
  rw [Nat.mul_add, Nat.add_mul, Nat.add_mul, ←Nat.add_assoc]

example : (x + y) * (x + y) = x * x + y * x + x * y + y * y := by
  simp [Nat.mul_add, Nat.add_mul, Nat.add_assoc]
