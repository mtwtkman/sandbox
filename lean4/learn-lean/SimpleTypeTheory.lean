-- # https://lean-lang.org/theorem_proving_in_lean4/Dependent-Type-Theory/#dependent-type-theory
/- Define some constants. -/
def m : Nat := 1 -- m is a natual number
def n : Nat := 0
def b1 : Bool := true -- b1 is a boolean
def b2 : Bool := false

/- Check their types. -/
#check m
#check n
#check n + 0
#check m * (n + 0)
#check b1
#check b1 && b2
#check b1 || b2
#check true

/- Evalueate -/
#eval 5 * 4
#eval m + 2
#eval b1 && b2
#eval b1 && b2

#check Nat → Nat
#check Nat × Nat
#check Prod Nat Nat
#check Nat -> Nat -> Nat
#check Nat -> (Nat -> Nat)
#check Nat × Nat -> Nat
#check (Nat -> Nat) -> Nat

#check Nat.succ
#check (0, 1)
#check Nat.add

#check Nat.succ 2
#check Nat.add 3
#check Nat.add 5 2
#check (5, 9).1
#check (5, 9).2

#eval Nat.succ 2
#eval Nat.add 5 2
#eval (5, 9).1
#eval (5, 9).2

def α : Type := Nat
def β : Type := Bool
def F : Type → Type := List
def G : Type → Type → Type := Prod

#check α
#check F α
#check F Nat
#check G α
#check G α β
#check G α Nat

#check Prod α β
#check α × β
#check Prod Nat Nat
#check Nat × Nat
#check List α
#check List Nat

#check Type
#check Type 1
#check Type 0
#check List
#check Prod

universe u

def F2 (α : Type u) : Type u := Prod α α
#check F2

def F3.{v} (α : Type v) : Type v := Prod α α
#check F3

#check fun (x : Nat) => x + 5
#check λ (x : Nat) => x + 5

#check fun x => x + 5
#check λ x => x + 5

#eval (λ x : Nat => x + 5) 10

#check fun x : Nat => fun y : Bool => if not y then x + 1 else x
#check fun (x: Nat) (y : Bool) => if not y then x + 1 else x + 2
#check fun x y => if not y then x + 1 else x + 2

def f (n : Nat) : String := toString n
def g (s : String) : Bool := s.length > 0

#check fun x : Nat => x
#check fun x : Nat => true
#check fun x : Nat => g (f x)
#check fun x => g (f x)
#check fun (g : String -> Bool) (f : Nat -> String) (x : Nat) => g (f x)
#check fun (α β γ : Type) ( g : β -> γ ) (f : α -> β) (x : α) => g (f x)

#check (fun x : Nat => x) 1
#check (fun x : Nat => true) 1

#check (fun (α β γ : Type) (μ : β -> γ) (v : α -> β) (x : α) => μ (v x))

#eval (fun x : Nat => x) 1
#eval (fun x : Nat => true) 1

def double (x : Nat) : Nat := x + x
#eval double 3

def double2 : Nat -> Nat := fun x => x + x
#eval double 3

def double3 := fun (x : Nat) => x + x

def pi := 3.141592654

def add (x y : Nat) := x + y
#eval add 3 2

def add2 (x : Nat) (y : Nat) := x + y
#eval add2 (double 3) (7 + 9)

def greater (x y : Nat) := if x > y then x else y

def doTwice (f : Nat -> Nat) (x : Nat) : Nat := f (f x)
#eval doTwice double 2

def compose (α β γ : Type) (g : β -> γ) (f : α -> β) (x : α) : γ := g (f x)

def square (x : Nat) : Nat := x * x

#eval compose Nat Nat Nat double square 3

#check let y := 2 + 2; y * y
#eval let y := 2 + 2; y * y

def twice_double (x : Nat) : Nat := let y := x + x; y * y
#eval twice_double 2

#check let y := 2 + 2; let z := y + y; z * z
#eval let y := 2 + 2; let z := y + y; z * z

def t (x : Nat) : Nat :=
  let y := x + x
  y * y

def foo := let a := Nat; fun x : a => x + 2

def doThrice (α : Type) (h : α -> α ) (x : α ) : α :=
  h (h (h x))

variable (α β γ : Type)
def compose2 (g : β -> γ) (f : α -> β) (x : α) : γ :=
  g (f x)
def doTwice2 (h : α -> α) (x : α) : α := h (h x)
def doThrice2 (h : α -> α) (x : α) : α := h (h (h x))

variable (g : β -> γ) (f : α -> β) (h : α -> α)
variable (x : α)

def compose3 := g (f x)
def doTwice3 := h (h x)
def doThrice3 := h (h (h x))

#print compose3
#print doTwice3
#print doThrice3

section useful
  variable (α β γ : Type)
  variable (g : β -> γ) (f : α -> β) (h : α -> α)
  variable (x : α)

  def compose4 := g (f x)
  def doTwice4 := h (h x)
  def doThrice4 := h (h (h x))
end useful

namespace Foo
  def a : Nat := 5
  def f2 (x: Nat) : Nat := x + 7

  def fa : Nat := f2 a
  namespace Bar
    def ffa : Nat := f2 (f2 a)

    #check a
    #check f2
    #check fa
    #check ffa
    #check Foo.fa
  end Bar

end Foo

#check Foo.a
#check Foo.f2
#check Foo.fa
#check Foo.Bar.ffa

open Foo

#check a
#check f2
#check fa
#check Foo.fa
#check Bar.ffa

#check List.nil
#check List.cons
#check List.map

def cons (α : Type) (a : α) (as : List α) : List α :=
  List.cons a as

#check cons Nat
#check cons Bool
#check cons

#check @List.cons
#check @List.nil
#check @List.length
#check @List.append

universe v

def f2 (α : Type u) (β : α -> Type v) (a : α) (b : β a) : (a : α) × β a := ⟨a, b⟩
def g2 (α : Type u) ( β : α -> Type v) (a : α) (b : β a) : Σ a : α , β a := Sigma.mk a b
def h1 (x : Nat) : Nat := (f2 Type (fun a => a) Nat x).2
#eval h1 5

def h2 (x : Nat) : Nat := (g2 Type (fun a => a) Nat x).2
#eval h2 5

def Lst (α : Type u) : Type u := List α

def Lst.cons {α : Type u} (a : α) (as : Lst α) : Lst α := List.cons a as
def Lst.nil {α : Type u} : Lst α := List.nil
def Lst.append {α : Type u} (as bs : Lst α) : Lst α := List.append as bs
#check Lst.cons 0 Lst.nil

def as : Lst Nat := Lst.nil
def bs : Lst Nat := Lst.cons 5 Lst.nil

#check Lst.append as bs

def ident {α : Type u} (x : α) := x

#check (ident)
#check ident 1
#check ident "hello"
#check @ident

section
  variable {α : Type u}
  variable (x : α)
  def ident2 := x
end

#check ident2
#check ident2 4
#check ident2 "hello"
