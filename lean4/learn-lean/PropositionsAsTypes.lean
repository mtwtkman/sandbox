-- https://lean-lang.org/theorem_proving_in_lean4/Propositions-and-Proofs/#propositions-and-proofs
#check And
#check Or
#check Not
variable (p q r : Prop)

#check And p q
#check Or (And p q) r

-- adhoc
def Proof (p : Prop) : Type := Prop
#check Proof

def Implies (p : Prop) (q : Prop) : Prop := And p q
#check Implies
#check Implies (And p q) (And q p)

axiom and_commut (p q : Prop) : Proof (Implies (And p q) (And q p))
#check and_commut p q

axiom modus_ponens (p q : Prop) :
    Proof (Implies p q) -> Proof p ->
    Proof q

axiom implies_intro (p q : Prop) :
    (Proof p -> Proof q) -> Proof (Implies p q)

set_option linter.unusedVariables false
---
variable {p: Prop}
variable {q: Prop}

theorem t1 : p -> q -> p := fun hp : p => fun hq : q => show p from hp
#print t1

theorem t2 (hp : p) (hq : q) : p := hp
#print t2

axiom hp : p
theorem t3 : q -> p := t1 hp

axiom unsound : False
-- Everything follows from false
theorem ex : 1 = 0 :=
  False.elim unsound

theorem t4 : ∀ {p q : Prop}, p -> q -> p :=
  fun {p q : Prop} (hp : p) (hq : q) => hp

variable (p q r : Prop)

theorem t5 (h₁ : q -> r) (h₂ : p -> q) : p -> r :=
  fun h₃ : p =>
    show r from h₁ (h₂ h₃)
#print t5

#check p → q → p ∧ q
#check ¬ p → p ↔ False
#check p ∨ q → q ∨ p

example (hp : p) (hq : q) : p ∧ q := And.intro hp hq
#check fun (hp : p) (hq : q) => And.intro hp hq

example (h : p ∧ q) : p := And.left h
example (h : p ∧ q) : q := And.right h
example (h : p ∧ q) : q ∧ p :=
  And.intro (And.right h) (And.left h)
