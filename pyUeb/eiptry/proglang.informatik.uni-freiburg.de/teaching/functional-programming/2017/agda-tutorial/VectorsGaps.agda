module VectorsGaps where

open import Numbers

data _≤_ : ℕ → ℕ → Set where
  z≤n : {n : ℕ} → zero ≤ n
  s≤s : {m n : ℕ} → m ≤ n → suc m ≤ suc n

data Vec (A : Set) : (n : ℕ) → Set where
  Nil  : Vec A zero
  Cons : {n : ℕ} → (a : A) → Vec A n → Vec A (suc n)

concat : ∀ {A m n} → Vec A m → Vec A n → Vec A (add m n)
concat Nil ys = ys
concat (Cons a xs) ys = Cons a (concat xs ys)

get : ∀ {A n} → Vec A n → (m : ℕ) → suc m ≤ n → A
get xs m = {!!}


--------------------------------------------------------------------------------
-- finite sets 0 .. n-1
--------------------------------------------------------------------------------
data Fin : ℕ → Set where
  zero : {n : ℕ} → Fin (suc n)
  suc  : {n : ℕ} → Fin n → Fin (suc n)

-- get using finite set
get1 : ∀ {A n} → Vec A n → Fin n → A
get1 xs i = {!!}

-- Pair
data _×_ (A B : Set) : Set where
  _,_ : (a : A) → (b : B) → (A × B)

-- split a vector in two parts
split : ∀ {A n} → Vec A n → (m : ℕ) → m ≤ n → Vec A m × Vec A (sub n m)
split xs m m≤n = {!!}

{-
-- alternative using Fin
split1 : ∀ {A n} → Vec A n → (m : Fin n) → Vec A m × Vec A (sub n m)
split1 xs m = ?
-}

--------------------------------------------------------------------------------
-- task
--------------------------------------------------------------------------------
-- define a function chunk  that computes a list of m-element chunks from a vector of length n
-- for simplicity, omit the remaining elements

-- you will need some help (besides the split function)

-- lists (vectors without length)
data List (A : Set) : Set where
  Nil  : List A
  Cons : (a : A) → List A → List A

-- sums (aka disjunction)
data _+_ (A B : Set) : Set where
  inl : (a : A) → A + B
  inr : (b : B) → A + B

-- a function that decides whether m ≤ n or m > n
check : (m n : ℕ) → (m ≤ n) + (suc n ≤ m)
check m n = {!!}

-- signature of chunk
chunk : ∀ {A} → (m n : ℕ) → Vec A n → List (Vec A m)
chunk m n xs = {!!}

-- chunking that keeps the remaining elements
data Σ (A : Set) (B : A → Set) : Set where
  ⟨_,_⟩ : (a : A) → (b : B a) → Σ A B

chunk1 : ∀ {A} → (m n : ℕ) → Vec A n → List (Vec A m) × Σ ℕ (λ l → Vec A l)
chunk1 m n xs = {!!}
