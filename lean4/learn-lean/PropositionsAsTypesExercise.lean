variable (p q r : Prop)

open Classical

example (p q : Prop) : ¬(p ∧ ¬q) → (p → q) :=
  fun h : ¬(p ∧ ¬q) =>
    fun hp : p =>
      show q from
        Or.elim (em q)
        (fun hq : q => hq)
        (fun hnq : ¬q => absurd (And.intro hp hnq) h)

theorem and_swap : p ∧ q ↔ q ∧ p :=
  Iff.intro
    (fun h : p ∧ q =>
      show q ∧ p from And.intro (And.right h) (And.left h))
    (fun h : q ∧ p =>
      show p ∧ q from And.intro (And.right h) (And.left h))

example (h : p ∨ q) : q ∨ p :=
  h.elim (fun hp => Or.inr hp) (fun hq => Or.inl hq)

-- commutativity of ∧ and ∨
example : p ∧ q ↔ q ∧ p :=
  Iff.intro
    (fun h : p ∧ q =>
      have hp : p := h.left
      have hq : q := h.right
      show q ∧ p from And.intro hq hp)
    (fun h : q ∧ p =>
      have hq : q := h.left
      have hp : p := h.right
      show p ∧ q from And.intro hp hq)

example : p ∨ q ↔ q ∨ p :=
  Iff.intro
    (fun h : p ∨ q =>
      show q ∨ p from Or.elim h (fun hp => Or.inr hp) (fun hq => Or.inl hq))
    (fun h : q ∨ p =>
      show p ∨ q from Or.elim h (fun hq => Or.inr hq) (fun hp => Or.inl hp))

-- associativity of ∧ and ∨
example : (p ∧ q) ∧ r ↔ p ∧ (q ∧ r) :=
  Iff.intro
    (fun h : (p ∧ q) ∧ r =>
      have hp : p := h.left.left
      have hq : q := h.left.right
      have hr : r := h.right
      show p ∧ (q ∧ r) from ⟨hp,⟨hq,hr⟩⟩)
    (fun h: p ∧ (q ∧ r) =>
      have hp : p := h.left
      have hq : q := h.right.left
      have hr : r := h.right.right
      show (p ∧ q) ∧ r from ⟨⟨hp, hq⟩,hr⟩)

example : (p ∨ q) ∨ r ↔ p ∨ (q ∨ r) :=
  Iff.intro
    (fun h : (p ∨ q) ∨ r =>
      show p ∨ (q ∨ r) from Or.elim h
        (fun h' : p ∨ q =>
          Or.elim h'
            (fun hp : p => Or.inl hp)
            (fun hq : q => Or.inr (Or.inl hq)))
        (fun hr : r => Or.inr (Or.inr hr)))
    (fun h : p ∨ (q ∨ r) =>
      show (p ∨ q) ∨ r from Or.elim h
        (fun hp : p => Or.inl (Or.inl hp))
        (fun h' : q ∨ r =>
          Or.elim h'
            (fun hq : q => Or.inl (Or.inr hq))
            (fun hr : r => Or.inr hr)))

-- distributivity
example : p ∧ (q ∨ r) ↔ (p ∧ q) ∨ (p ∧ r) :=
  Iff.intro
    (fun h : p ∧ (q ∨ r) =>
      have hp : p := h.left
      show (p ∧ q) ∨ (p ∧ r) from Or.elim h.right
          (fun hq : q => Or.inl ⟨hp, hq⟩)
          (fun hr : r => Or.inr ⟨hp, hr⟩))
    (fun h : (p ∧ q) ∨ (p ∧ r) =>
      show p ∧ (q ∨ r) from Or.elim h
        (fun h' : p ∧ q  => ⟨h'.left, Or.inl h'.right⟩)
        (fun h' : p ∧ r => ⟨h'.left, Or.inr h'.right⟩))

example : p ∨ (q ∧ r) ↔ (p ∨ q) ∧ (p ∨ r) :=
  Iff.intro
    (fun h : p ∨ (q ∧ r) =>
      show (p ∨ q) ∧ (p ∨ r) from Or.elim h
        (fun hp : p => ⟨Or.inl hp, Or.inl hp⟩)
        (fun h' : q ∧ r => ⟨Or.inr h'.left, Or.inr h'.right⟩))
    (fun h : (p ∨ q) ∧ (p ∨ r) =>
      show p ∨ (q ∧ r) from Or.elim h.left
        (fun hp : p => Or.inl hp)
        (fun hp : q =>
          Or.elim h.right
            (fun hp : p => Or.inl hp)
            (fun hr : r => Or.inr ⟨hp, hr⟩)))

-- other properties
example : (p → (q → r)) ↔ (p ∧ q → r) :=
    Iff.intro
      (λ h : p → (q → r) =>
        λ h' : p ∧ q => (h h'.left) h'.right
      )
      (λ h : p ∧ q → r =>
        λ hp : p =>
          λ hq : q =>
            h ⟨hp, hq⟩)

example : ((p ∨ q) → r) ↔ (p → r) ∧ (q → r) :=
  Iff.intro
    (λ h : (p ∨ q) → r =>
      And.intro
        (λ hp : p => h (Or.inl hp))
        (λ hq : q => h (Or.inr hq)))
    (λ h : (p → r) ∧ (q → r) =>
      λ h' : p ∨ q => Or.elim h' (λ hp : p => h.left hp) (λ hq : q => h.right hq))

example : ¬(p ∨ q) ↔ ¬p ∧ ¬q :=
  Iff.intro
      (λ h : ¬(p ∨ q) =>
        show ¬p ∧ ¬q from ⟨λ hp : p => show False from h (Or.inl hp)
        ,λ hq : q => show False from h (Or.inr hq)⟩)
      (λ h : ¬p ∧ ¬q =>
        show ¬(p ∨ q) from
          λ h' : p ∨ q =>
            Or.elim h'
              (λ hp : p => h.left hp)
              (λ hq : q => h.right hq))

example : ¬p ∨ ¬q → ¬(p ∧ q) :=
  fun h : ¬p ∨ ¬q =>
    λ h' : p ∧ q =>
      Or.elim h
        (λ hnp : ¬p => hnp h'.left)
        (λ hnq : ¬q => hnq h'.right)

example : ¬(p ∧ ¬p) :=
  λ h : p ∧ ¬p => h.right h.left

example : p ∧ ¬q → ¬(p → q) :=
  λ h : p ∧ ¬q =>
    λ h' : p → q =>
      have hnq : ¬q := h.right
      have hq : q := h' h.left
      show False from hnq hq

example : ¬p → (p → q) :=
  λ hnp : ¬p =>
    λ hp : p => absurd hp hnp

example : (¬p ∨ q) → (p → q) :=
  λ h : ¬p ∨ q =>
    λ hp: p =>
      Or.elim h
        (fun hnp : ¬p => False.elim (hnp hp))
        (fun hq : q => hq)

example : p ∨ False ↔ p :=
  Iff.intro
    (fun h : p ∨ False =>
      show p from Or.elim h
        (λ hp : p => hp)
        (False.elim))
    Or.inl

example : p ∧ False ↔ False :=
  Iff.intro
    (fun h : p ∧ False => h.right)
    False.elim

example : (p → q) → (¬q → ¬p) :=
  λ h : p → q =>
    λ hnq : ¬q =>
      λ hp : p =>
        absurd (h hp) hnq

-- classical
example : (p → q ∨ r) → ((p → q) ∨ (p → r)) :=
  λ h : p → q ∨ r =>
    Or.elim (em q)
      (λ hq : q => Or.inl (λ hp : p => hq))
      (λ hnq : ¬q =>
        Or.inr
          (λ hp : p =>
            Or.elim (h hp)
            (λ hq : q => absurd hq hnq)
            (λ hr : r => hr)))

example : ¬(p ∧ q) → ¬p ∨ ¬q := sorry

example : ¬(p → q) → p ∧ ¬q := sorry

example : (p → q) → (¬p ∨ q) := sorry

example : (¬q → ¬p) → (p → q) := sorry

example : p ∨ ¬p := sorry

example : (((p → q) → p) → p) := sorry
