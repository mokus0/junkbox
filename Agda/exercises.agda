module Exercises where

open import Data.Bool using (Bool; true; false; T)
open import Data.Nat  using (ℕ; zero; suc; _+_)
open import Data.List using (List; []; _∷_; length)
open import Data.Vec  using (Vec; []; _∷_)
open import Data.Product using (_×_; _,_)
open import Relation.Binary.PropositionalEquality as PropEq
  using (_≡_; refl)

{-
    Exercises from "Dependently Typed Programming in Agda"
 -}

-- Exercise 2.1 Matrix transposition

Matrix : Set →  ℕ → ℕ → Set
Matrix A n m = Vec (Vec A n) m

-- (a) Define a function to compute a vector containing /n/ copies of an element x

vec : {n : ℕ}{A : Set} → A → Vec A n
vec {zero } _ = []
vec {suc n} x = x ∷ vec x


-- (b) Define pointwise application of a vector of functions to a vector of arguments

infixl 90 _<*>_

_<*>_ : {n : ℕ}{A B : Set} → Vec (A -> B) n -> Vec A n -> Vec B n
[] <*> [] = []
(f ∷ fs) <*> (x ∷ xs) = f x ∷ fs <*> xs

-- (c) Define matrix transposition in terms of these two functions.

transpose : ∀ {A n m} → Matrix A n m -> Matrix A m n
transpose [] = vec []
transpose (xs ∷ xss) = vec _∷_ <*> xs <*> transpose xss


xss = (1 ∷ 2 ∷ 3 ∷ [])
    ∷ (4 ∷ 5 ∷ 6 ∷ [])
    ∷ []
yss = (1 ∷ 4 ∷ [])
    ∷ (2 ∷ 5 ∷ [])
    ∷ (3 ∷ 6 ∷ [])
    ∷ []

test : yss ≡ transpose xss
test = refl


-- Exercise 3.1 Natural Numbers

data Compare : ℕ → ℕ -> Set where
    less : ∀ {n} k → Compare n (n + suc k)
    more : ∀ {m} k → Compare (m + suc k) m
    same : ∀ {n} → Compare n n

-- (a) Define the view function

compare : (n m : ℕ) -> Compare n m
compare zero zero    = same
compare zero (suc m) = less m
compare (suc n) zero = more n
compare (suc n) (suc m) with compare n m
compare (suc n) (suc .n) | same = same
compare (suc n) (suc .(n + suc k)) | less k = less k
compare (suc .(m + suc k)) (suc m) | more k = more k

-- (b) Now use the view to compute the difference between two numbers

difference : ℕ → ℕ → ℕ
difference n m with compare n m
difference n ._ | less k = suc k
difference ._ m | more k = suc k
difference n .n | same   = zero

-- Exercise 3.2 Type Checking λ-calculus

infixr 30 _⇒_
data Type : Set where
    unit : Type
    _⇒_  : Type -> Type -> Type

-- -- (a) Define inequality on types and change the type comparison to include inequality proofs.
data _≠_ : Type -> Type -> Set where
    unit≠func : ∀ {σ τ} → unit ≠ σ ⇒ τ
    func≠unit : ∀ {σ τ} → σ ⇒ τ ≠ unit
    
    src≠src   : ∀ {σ₁ σ₂ τ} → σ₁ ≠ σ₂ → σ₁ ⇒ τ  ≠ σ₂ ⇒ τ
    dst≠dst   : ∀ {σ τ₁ τ₂} → τ₁ ≠ τ₂ → σ  ⇒ τ₁ ≠ σ  ⇒ τ₂
    func≠func : ∀ {σ₁ σ₂ τ₁ τ₂} → σ₁ ≠ σ₂ → τ₁ ≠ τ₂ → σ₁ ⇒ τ₁ ≠ σ₂ ⇒ τ₂

data Equal? : Type -> Type -> Set where
    yes : ∀ {τ} → Equal? τ τ
    no  : ∀ {σ τ} → σ ≠ τ → Equal? σ τ

_=?=_ : (σ τ : Type) → Equal? σ τ
unit =?= unit       = yes
unit =?= σ ⇒ τ      = no unit≠func
σ ⇒ τ =?= unit      = no func≠unit

σ₁ ⇒ τ₁ =?= σ₂ ⇒ τ₂ with σ₁ =?= σ₂ | τ₁ =?= τ₂
σ  ⇒ τ  =?= .σ ⇒ .τ | yes      | yes      = yes
σ₁ ⇒ τ  =?= σ₂ ⇒ .τ | no σ₁≠σ₂ | yes      = no (src≠src σ₁≠σ₂)
σ  ⇒ τ₁ =?= .σ ⇒ τ₂ | yes      | no τ₁≠τ₂ = no (dst≠dst τ₁≠τ₂)
σ₁ ⇒ τ₁ =?= σ₂ ⇒ τ₂ | no σ₁≠σ₂ | no τ₁≠τ₂ = no (func≠func σ₁≠σ₂ τ₁≠τ₂)

-- (b) Define a type of illtyped terms and change infer to return such a term 
-- upon failure.  Look to the definition of infer for clues to the constructors
-- of BadTerm

infixl 80 _$_
data Raw : Set where
    var : ℕ → Raw
    _$_ : Raw → Raw → Raw
    lam : Type → Raw → Raw

Cxt = List Type

data _∈_ {A : Set}(x : A) : List A -> Set where
    hd : ∀ {xs}             → x ∈ (x ∷ xs)
    tl : ∀ {y xs} → x ∈ xs -> x ∈ (y ∷ xs)

index : {A : Set}{x : A}{xs : List A} → x ∈ xs → ℕ
index hd = zero
index (tl x∈xs) = suc (index x∈xs)

data Lookup {A : Set}(xs : List A) : ℕ → Set where
    inside  : (x : A)(p : x ∈ xs) → Lookup xs (index p)
    outside : (m : ℕ) → Lookup xs (length xs + m)

_!_ : {A : Set}(xs : List A)(n : ℕ) → Lookup xs n
[]       ! m    = outside m
(x ∷ xs) ! zero = inside x hd
(x ∷ xs) ! suc n with xs ! n
(x ∷ xs) ! suc .(index p)       | inside y p  = inside y (tl p)
(x ∷ xs) ! suc .(length xs + m) | outside m   = outside m

data Term (Γ : Cxt) : Type -> Set where
    var : ∀ {τ} → τ ∈ Γ → Term Γ τ
    _$_ : ∀ {σ τ} → Term Γ (σ ⇒ τ) → Term Γ σ → Term Γ τ
    lam : ∀ σ {τ} → Term (σ ∷ Γ) τ → Term Γ (σ ⇒ τ)

erase : {Γ : Cxt}{τ : Type} → Term Γ τ → Raw
erase (var n) = var (index n)
erase (e₁ $ e₂) = erase e₁ $ erase e₂
erase (lam σ e) = lam σ (erase e)

data BadTerm (Γ : Cxt) : Set where
    var∉Γ : ℕ → BadTerm Γ
    not-a-func : ∀ {τ} → Term Γ unit → Term Γ τ  → BadTerm Γ
    not-a-func+bad-arg : Term Γ unit → BadTerm Γ → BadTerm Γ
    type-mismatch  : ∀ {σ₁ σ₂ τ} → σ₁ ≠ σ₂ → Term Γ (σ₁ ⇒ τ) → Term Γ σ₂ → BadTerm Γ
    bad-func : ∀ {τ} → BadTerm Γ → Term Γ τ → BadTerm Γ
    bad-arg  : ∀ {σ τ} → Term Γ (σ ⇒ τ) → BadTerm Γ → BadTerm Γ
    bad-$    : BadTerm Γ → BadTerm Γ → BadTerm Γ
    bad-lam  : ∀ τ → BadTerm (τ ∷ Γ) → BadTerm Γ
    -- ...
    
    -- and kill this one
    foo : Raw → BadTerm Γ

eraseBad : {Γ : Cxt} → BadTerm Γ → Raw
eraseBad {Γ} (var∉Γ m) = var (length Γ + m)
eraseBad (foo raw) = raw
eraseBad (not-a-func          t₁ t₂) = erase t₁ $ erase    t₂
eraseBad (not-a-func+bad-arg  t₁ t₂) = erase t₁ $ eraseBad t₂
eraseBad (type-mismatch σ₁≠σ₂ t₁ t₂) = erase t₁ $ erase    t₂
eraseBad (bad-func t₁ t₂) = eraseBad t₁ $ erase    t₂
eraseBad (bad-arg  t₁ t₂) = erase    t₁ $ eraseBad t₂
eraseBad (bad-$    t₁ t₂) = eraseBad t₁ $ eraseBad t₂
eraseBad (bad-lam τ t) = lam τ (eraseBad t)

data Infer (Γ : Cxt) : Raw → Set where
    ok  : (τ : Type)(t : Term Γ τ) → Infer Γ (erase t)
    bad : (b : BadTerm Γ) → Infer Γ (eraseBad b)

infer : (Γ : Cxt)(e : Raw) → Infer Γ e
infer Γ (var n) with Γ ! n
infer Γ (var .(index p))      | inside τ p  = ok τ (var p)
infer Γ (var .(length Γ + m)) | outside m   = bad (var∉Γ m)

infer Γ (e₁ $ e₂) with infer Γ e₁ | infer Γ e₂ 
infer Γ (.(erase    t₁) $ .(erase    t₂)) | ok unit     t₁ | ok σ₂ t₂ = bad (not-a-func t₁ t₂)
infer Γ (.(erase    t₁) $ .(eraseBad t₂)) | ok unit     t₁ | bad t₂   = bad (not-a-func+bad-arg t₁ t₂)
infer Γ (.(erase    t₁) $ .(erase    t₂)) | ok (σ₁ ⇒ τ) t₁ | ok σ₂ t₂ with σ₁ =?= σ₂
infer Γ (.(erase    t₁) $ .(erase    t₂)) | ok (σ  ⇒ τ) t₁ | ok .σ t₂ | yes       = ok τ (t₁ $ t₂)
infer Γ (.(erase    t₁) $ .(erase    t₂)) | ok (σ₁ ⇒ τ) t₁ | ok σ₂ t₂ | no σ₁≠σ₂  = bad (type-mismatch σ₁≠σ₂ t₁ t₂)
infer Γ (.(eraseBad t₁) $ .(erase    t₂)) | bad         t₁ | ok τ  t₂ = bad (bad-func t₁ t₂)
infer Γ (.(erase    t₁) $ .(eraseBad t₂)) | ok (σ₁ ⇒ τ) t₁ | bad   t₂ = bad (bad-arg t₁ t₂)
infer Γ (.(eraseBad t₁) $ .(eraseBad t₂)) | bad         t₁ | bad   t₂ = bad (bad-$ t₁ t₂)

infer Γ (lam σ e) with infer (σ ∷ Γ) e
infer Γ (lam σ .(erase    t)) | ok τ t = ok (σ ⇒ τ) (lam σ t)
infer Γ (lam σ .(eraseBad t)) | bad t   = bad (bad-lam σ t)

-- some test terms
-- s = λx:σ y:τ₁ z:τ₂. x z (y z)
s : Type → Type → Type → Raw
s σ τ₁ τ₂ = 
    lam (σ ⇒ τ₁ ⇒ τ₂) 
        (lam (σ ⇒ τ₁) 
            (lam σ (var 2 $ var 0 $ (var 1 $ var 0))))

-- k = λx:σ y:τ. x
k : Type → Type → Raw
k σ τ = lam σ (lam τ (var 1))

-- Exercise 3.3. Properties of list functions

data All {A : Set}(P : A → Set) : List A → Set where
    [] : All P []
    _∷_ : ∀ {x xs} → P x → All P xs → All P (x ∷ xs)

-- (a) prove the following lemma stating that All is sound:
lemma-All-∈ : ∀ {A x xs}{P : A → Set} → All P xs → x ∈ xs → P x
lemma-All-∈         []       ()
lemma-All-∈ {x = x} (p ∷ ps) hd         = p
lemma-All-∈         (p ∷ ps) (tl x∈xs)  = lemma-All-∈ ps x∈xs

-- (b) Below is the proof that all elements of 'filter p xs' satisfies p.  Doing
-- this without any auxilliary lemmas involves some rather subtle use of with-
-- abstraction.  Figure out what is going on by replaying the construction of the 
-- program and looking at the goal and context in each step.

filter : {A : Set} → (A → Bool) → List A → List A
filter p [] = []
filter p (x ∷ xs) with p x
... | true  = x ∷ filter p xs
... | false =     filter p xs

satisfies : {A : Set} → (A → Bool) → A → Set
satisfies p x = T (p x)

trueIsTrue : {x : Bool} → x ≡ true → T x
trueIsTrue refl = _

data Inspect {A : Set}(x : A) : Set where
    it : (y : A) → x ≡ y → Inspect x
inspect : {A : Set}(x : A) → Inspect x
inspect x = it x refl

lem-filter-sound : {A : Set}(p : A → Bool)(xs : List A) → All (satisfies p) (filter p xs)
lem-filter-sound p [] = []
lem-filter-sound p (x ∷ xs) with inspect (p x)
lem-filter-sound p (x ∷ xs) | it y prf with p x | prf
lem-filter-sound p (x ∷ xs) | it .true  prf | true  | refl = trueIsTrue prf ∷ lem-filter-sound p xs
lem-filter-sound p (x ∷ xs) | it .false prf | false | refl = lem-filter-sound p xs

-- (c) Finally prove filter complete, by proving that all elements of the
-- original list satisfying the predicate are in the result.

lem-filter-complete : {A : Set}(p : A → Bool)(x : A){xs : List A} → 
    x ∈ xs → satisfies p x → x ∈ filter p xs
lem-filter-complete _ _ {[]} () _
lem-filter-complete p x {.x ∷ xs} hd       px with p x
lem-filter-complete p x {.x ∷ xs} hd       px | true
    = hd
lem-filter-complete p x {.x ∷ xs} hd       () | false
lem-filter-complete p x {y ∷ xs} (tl x∈xs) px with p y
lem-filter-complete p x {y ∷ xs} (tl x∈xs) px | true
    = tl (lem-filter-complete p x {xs} x∈xs px)
lem-filter-complete p x {y ∷ xs} (tl x∈xs) px | false
    = lem-filter-complete p x {xs} x∈xs px

