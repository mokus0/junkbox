module polyrelevance where

open import Data.Bool
open import Relation.Binary.PropositionalEquality

record irr {a}(A : Set a) : Set a where
  constructor
    this
  field
    .that : A

record rel {a}(A : Set a) : Set a where
  constructor
    this
  field
    that : A

relevant : Bool → ∀{a} → Set a → Set a
relevant true  = irr
relevant false = rel

relUnit : ∀ {a}{A : Set a} → (rel : Bool) → A → relevant rel A
relUnit true  = this
relUnit false = this

relApp : ∀ {a b}{A : Set a}{B : Set b} (rel : Bool) → relevant rel (A → B) → relevant rel A → relevant rel B
relApp true  (this f) (this x) = this (f x)
relApp false (this f) (this x) = this (f x)

relMap : ∀ {a b}{A : Set a}{B : Set b} (rel : Bool) → (A → B) → relevant rel A → relevant rel B
relMap rel f = relApp rel (relUnit rel f)

relJoin : ∀ {a}{A : Set a} → (rel : Bool) → relevant rel (relevant rel A) → relevant rel A
relJoin true  (this (this x)) = this x
relJoin false (this (this x)) = this x

relBind : ∀ {a b}{A : Set a}{B : Set b} (rel : Bool) → (A → relevant rel B) → relevant rel A → relevant rel B
relBind rel f x = relJoin rel (relMap rel f x)

relEq : ∀ {a}{A B : Set a} → (rel : Bool) → relevant rel (A ≡ B) → relevant rel A ≡ relevant rel B
relEq false (this refl) = refl
relEq true  (this A≡B)  = A≡B₂ A≡B
  where
    A≡B₂ : ∀ {a}{A B : Set a} → .(A ≡ B) → irr A ≡ irr B
    A≡B₂ = ?