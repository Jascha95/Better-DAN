module LogicGaps where

-- Truth
data ⊤ : Set where
  tt : ⊤

-- Conjunction
data _∧_ (P Q : Set) : Set where
  ⟨_,_⟩ : (p : P) → (q : Q) → (P ∧ Q)

-- Disjunction
data _∨_ (P Q : Set) : Set where
  inl : (p : P) → (P ∨ Q)
  inr : (q : Q) → (P ∨ Q)

-- Conjunction is commutative
commConj1 : (P : Set) → (Q : Set) → P ∧ Q → Q ∧ P
commConj1 = {!!}









-- Disjunction is commutative
commDisj : ∀ {P Q} → (P ∨ Q) → (Q ∨ P)
commDisj = {!!}










-- Distributivity: conjunction over disjunction
distrCD1 : ∀ {P Q R} → (P ∨ Q) ∧ R → (P ∧ R) ∨ (Q ∧ R)
distrCD1 = {!!}

-- Distributivity: backwards direction
-- your job

-- Distributivity: disjunction over conjunction
-- your job

----------------------------------------

-- Falsity
data ⊥ : Set where

-- Negation
¬ : Set → Set
¬ P = P → ⊥

-- DeMorgan's laws, part I
demND1 : ∀ {P Q} → ¬ (P ∨ Q) → (¬ P ∧ ¬ Q)
demND2 : ∀ {P Q} → (¬ P ∧ ¬ Q) → ¬ (P ∨ Q)

demND1 = {!!}
demND2 = {!!}

-- DeMorgan's laws, part II
demNC1 : ∀ {P Q} → ¬ (P ∧ Q) → (¬ P ∨ ¬ Q)
demNC2 : ∀ {P Q} → (¬ P ∨ ¬ Q) → ¬ (P ∧ Q)

demNC1 = {!!}
demNC2 = {!!}

