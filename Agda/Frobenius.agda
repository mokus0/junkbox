module Frobenius where

open import Data.Product
    using (∃ ; _,_)
    renaming (_×_ to _∧_)

frob₁ : {A φ : Set}{ψ : A → Set} → φ ∧ (∃ ψ) → ∃ (λ x → φ ∧ ψ x)
frob₁ (φ , (x , ψx)) = (x , (φ , ψx))

frob₂ : {A φ : Set}{ψ : A → Set} → ∃ (λ x → φ ∧ ψ x) → φ ∧ (∃ ψ)
frob₂ (x , (φ , ψx)) = (φ , (x , ψx))