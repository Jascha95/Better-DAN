module Generic where

open import Data.Nat
open import Data.Fin hiding (_+_)
open import Data.List hiding (sum)
open import Relation.Binary.PropositionalEquality

data Type (n : ℕ) : Set where
  var  : Fin n → Type n
  unit : Type n
  void : Type n
  prod : Type n → Type n → Type n
  sum  : Type n → Type n → Type n
  fun  : Type n → Type n → Type n

mutual
  data Positive : Type 1 → Set where
    var :  Positive (var zero)
    unit : Positive unit
    void : Positive void
    prod : ∀ {s t} → Positive s → Positive t → Positive (prod s t)
    sum  : ∀ {s t} → Positive s → Positive t → Positive (sum s t)
    fun  : ∀ {s t} → Negative s → Positive t → Positive (fun s t)

  data Negative : Type 1 → Set where
    unit : Negative unit
    void : Negative void
    prod : ∀ {s t} → Negative s → Negative t → Negative (prod s t)
    sum  : ∀ {s t} → Negative s → Negative t → Negative (sum s t)
    fun  : ∀ {s t} → Positive s → Negative t → Negative (fun s t)

Env = List (Type 0)

data _∈_ : Type 0 → Env → Set where
  here : ∀ {t ρ} → t ∈ (t ∷ ρ)
  below : ∀ {t₁ t₂ ρ} → t₁ ∈ ρ → t₁ ∈ (t₂ ∷ ρ)

data Exp : Env → Type 0 → Set where
  var : ∀ {t ρ} → t ∈ ρ → Exp ρ t
  lam : ∀ {s t ρ} → Exp (s ∷ ρ) t → Exp ρ (fun s t)
  app : ∀ {s t ρ} → Exp ρ (fun s t) → Exp ρ s → Exp ρ t
  ⟨⟩ : ∀ {ρ} → Exp ρ unit
  abort : ∀ {ρ t} → Exp ρ void → Exp ρ t
  pair : ∀ {s t ρ} → Exp ρ s → Exp ρ t → Exp ρ (prod s t)
  π₁ : ∀ {s t ρ} → Exp ρ (prod s t) → Exp ρ s
  π₂ : ∀ {s t ρ} → Exp ρ (prod s t) → Exp ρ t
  ι₁ : ∀ {s t ρ} → Exp ρ s → Exp ρ (sum s t)
  ι₂ : ∀ {s t ρ} → Exp ρ t → Exp ρ (sum s t)
  case : ∀ {r s t ρ} → Exp ρ (sum s t) → Exp (s ∷ ρ) r → Exp (t ∷ ρ) r → Exp ρ r

ex1 : Exp ([]) (fun unit unit)
ex1 = lam (var here)

weakenIndex : ∀ {ρ₂ t s} → (ρ₁ : Env) → t ∈ (ρ₁ ++ ρ₂) → t ∈ (ρ₁ ++ s ∷ ρ₂)
weakenIndex [] x = below x
weakenIndex (s₁ ∷ ρ₁) here = here
weakenIndex (s₁ ∷ ρ₁) (below x) = below (weakenIndex ρ₁ x)

weaken : ∀ {ρ₂ t s} → (ρ₁ : Env) → Exp (ρ₁ ++ ρ₂) t → Exp (ρ₁ ++ s ∷ ρ₂) t
weaken ρ₁ (var x) = var (weakenIndex ρ₁ x)
weaken ρ₁ (lam e) = lam (weaken (_ ∷ ρ₁) e)
weaken ρ₁ (app e e₁) = app (weaken ρ₁ e) (weaken ρ₁ e₁)
weaken ρ₁ ⟨⟩ = ⟨⟩
weaken ρ₁ (abort e) = abort (weaken ρ₁ e)
weaken ρ₁ (pair e e₁) = pair (weaken ρ₁ e) (weaken ρ₁ e₁)
weaken ρ₁ (π₁ e) = π₁ (weaken ρ₁ e)
weaken ρ₁ (π₂ e) = π₂ (weaken ρ₁ e)
weaken ρ₁ (ι₁ e) = ι₁ (weaken ρ₁ e)
weaken ρ₁ (ι₂ e) = ι₂ (weaken ρ₁ e)
weaken ρ₁ (case e e₁ e₂) = case (weaken ρ₁ e) (weaken (_ ∷ ρ₁) e₁) (weaken (_ ∷ ρ₁) e₂)

⇑ : (n : ℕ) → Type 0 → Type n
⇑ n (var ())
⇑ n unit = unit
⇑ n void = void
⇑ n (prod s s₁) = prod (⇑ n s) (⇑ n s₁)
⇑ n (sum s s₁) = sum (⇑ n s) (⇑ n s₁)
⇑ n (fun s s₁) = fun (⇑ n s) (⇑ n s₁)

substitute : (n : ℕ) → Type (1 + n) → Type 0 → Type n
substitute n (var zero) s = ⇑ n s
substitute zero (var (suc ())) s
substitute (suc n) (var (suc x)) s = var x
substitute n unit s = unit
substitute n void s = void
substitute n (prod t t₁) s = prod (substitute n t s) (substitute n t₁ s)
substitute n (sum t t₁) s = sum (substitute n t s) (substitute n t₁ s)
substitute n (fun t t₁) s = fun (substitute n t s) (substitute n t₁ s)

up-0-r=r : (r : Type 0) → ⇑ 0 r ≡ r
up-0-r=r (var ())
up-0-r=r unit = refl
up-0-r=r void = refl
up-0-r=r (prod r r₁) rewrite up-0-r=r r | up-0-r=r r₁ = refl
up-0-r=r (sum r r₁) rewrite up-0-r=r r | up-0-r=r r₁ = refl
up-0-r=r (fun r r₁) rewrite up-0-r=r r | up-0-r=r r₁ = refl

subst0=id : (x : Fin 1) →  (r : Type 0) → substitute 0 (var x) r ≡ r
subst0=id zero r = up-0-r=r r
subst0=id (suc ()) r

W : ∀ {ρ₂ t s} → Exp ρ₂ t → Exp (s ∷ ρ₂) t
W = weaken []

-- generic extension for positive type operators
fmap : ∀ {r s ρ t} → Positive t → Exp ρ (fun r s)
     → Exp ρ (substitute 0 t r) → Exp ρ (substitute 0 t s)
gmap : ∀ {r s ρ t} → Negative t → Exp ρ (fun r s)
     → Exp ρ (substitute 0 t s) → Exp ρ (substitute 0 t r)

fmap {r} {s} {t = var x} p f a rewrite subst0=id x r | subst0=id x s = app f a
fmap unit f a = ⟨⟩
fmap void f a = abort a
fmap (prod p₁ p₂) f a =
     pair (fmap p₁ f (π₁ a)) (fmap p₂ f (π₂ a))
fmap (sum p₁ p₂) f a = 
     case a (ι₁ (fmap p₁ (W f) (var here))) (ι₂ (fmap p₂ (W f) (var here)))
fmap (fun n p) f a =
     lam (fmap p (W f) (app (W a) (gmap n (W f) (var here))))

gmap unit f a = ⟨⟩
gmap void f a = abort a
gmap (prod n₁ n₂) f a =
     pair (gmap n₁ f (π₁ a)) (gmap n₂ f (π₂ a))
gmap (sum n₁ n₂) f a =
     case a (ι₁ (gmap n₁ (W f) (var here))) (ι₂ (gmap n₂ (W f) (var here)))
gmap (fun p n) f a = 
     lam (gmap n (W f) (app (W a) (fmap p (W f) (var here))))

-- generic extension for arbitrary type operators with a pair of functions
fgmap : ∀ {r s ρ} → (t : Type 1)
      → Exp ρ (fun r s) → Exp ρ (fun s r)
      → Exp ρ (substitute 0 t r) → Exp ρ (substitute 0 t s)
fgmap {r} {s} (var x) f g a rewrite subst0=id x r | subst0=id x s = app f a
fgmap unit f g a = ⟨⟩
fgmap void f g a = abort a
fgmap (prod t₁ t₂) f g a =
  pair (fgmap t₁ f g (π₁ a)) (fgmap t₂ f g (π₂ a))
fgmap (sum t₁ t₂) f g a =
  case a (ι₁ (fgmap t₁ (W f) (W g) (var here)))
         (ι₂ (fgmap t₂ (W f) (W g) (var here)))
fgmap (fun t₁ t₂) f g a = 
  lam (fgmap t₂ (W f) (W g) (app (W a) (fgmap t₁ (W g) (W f) (var here))))

