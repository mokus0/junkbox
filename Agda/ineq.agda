{-# OPTIONS --universe-polymorphism #-}
-- Experiments with proving inequalities

module ineq where

open import Data.Nat
open import Data.Product
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary
import Level as L

_≠_ : ∀ {a}{α : Set a} → α → α → Set a
x ≠ y = ¬ (x ≡ y)

-- for Set, I would think it's pretty much trivial:
2≠3 : 2 ≠ 3
2≠3 ()

-- for other levels, not so sure...

data F2 : Set₁ where
  F2a : F2
  F2b : F2

data F3 : Set₁ where
  F3a : F3
  F3b : F3
  F3c : F3

total : ∀{l}{t : Set l}(x : t) → ∃ λ y → x ≡ y
total x = x , refl

coerce : ∀ {l}{x y : Set l} → x ≡ y → x → y
coerce refl x = x

F2≠F3 : F2 ≠ F3
F2≠F3 F2≡F3 with coerce (sym F2≡F3) F3c
F2≠F3 F2≡F3 | F2c with total F2c
F2≠F3 F2≡F3 | .F2a | F2a , refl = ?
F2≠F3 F2≡F3 | .F2b | F2b , refl = ?
