module NumbersGaps where

-- natural numbers
data ℕ : Set where
  zero : ℕ
  suc : (n : ℕ) → ℕ

-- addition
add : ℕ → ℕ → ℕ
add zero n = n
add (suc m) n = suc (add m n)

-- equality of natural numbers
data _==_ : ℕ → ℕ → Set where
  z==z : zero == zero
  s==s : {m n : ℕ} → m == n → suc m == suc n

----------------------------------------
-- properties of equality
----------------------------------------
-- reflexive
refl-== : (n : ℕ) → n == n
refl-== n = {!!}

-- symmetric
-- by pattern matching on m,n
symm1-== : (m n : ℕ) → m == n → n == m
symm1-== m n m==n = {!!}

-- by pattern matching on m==n
symm-== : {m n : ℕ} → m == n → n == m
symm-== m==n = {!!}

-- by pattern matching on m==n and n==o
trans-== : {m n o : ℕ} → m == n → n == o → m == o
trans-== m==n n==o = {!!}

----------------------------------------
-- tasks
----------------------------------------

-- define the relation "less than or equal" in analogy to equality
data _≤_ : ℕ → ℕ → Set where

-- properties: reflexivity and transitivity
refl-≤ : (n : ℕ) → n ≤ n
refl-≤ n = {!!}

trans-≤ : {m n o : ℕ} → m ≤ n → n ≤ o → m ≤ o
trans-≤ m≤n n≤o = {!!}

----------------------------------------
-- end tasks
----------------------------------------

-- zero is left neutral element of addition
neutralAdd0l : (m : ℕ) → add zero m == m
neutralAdd0l m = {!!}

-- zero is right neutral element of addition
neutralAdd0r : (m : ℕ) → add m zero == m
neutralAdd0r m = {!!}

-- addition is associative
assocAdd : (m n o : ℕ) → add m (add n o) == add (add m n) o
assocAdd m n o = {!!}

commAdd : (m n : ℕ) → add m n == add n m
commAdd m n = {!!}

----------------------------------------
-- tasks
----------------------------------------

-- consider subtraction
sub : ℕ → ℕ → ℕ
sub m zero = m
sub zero (suc n) = zero
sub (suc m) (suc n) = sub m n

-- zero is right neutral element of subtraction

-- subtraction cancels addition
subaddr : (m n : ℕ) → sub (add m n) n == m
subaddl : (m n : ℕ) → sub (add m n) m == n

subaddr = {!!}
subaddl = {!!}

----------------------------------------
-- end tasks
----------------------------------------
