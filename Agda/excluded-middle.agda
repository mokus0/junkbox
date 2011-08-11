-- a few simple classical theorems (under double negation).
module excluded-middle where

open import Relation.Nullary using (¬_)
open import Data.Empty using (⊥; ⊥-elim)
open import Data.Product 
    using (∃; ∄; _,_)
    renaming (_×_ to _∧_)

infixr 2 _∨_
data _∨_ (A B : Set) : Set where
    inl : A → A ∨ B
    inr : B → A ∨ B

private
    -- contradiction elimination
    contradiction-elim : {φ : Set} → ¬ (φ ∧ ¬ φ)
    contradiction-elim (φ , ¬φ) = ¬φ φ

    -- one of De Morgan's laws
    deMorgan₁ : {φ ψ : Set} → ¬ (φ ∨ ψ) → (¬ φ ∧ ¬ ψ)
    deMorgan₁ ¬[φ∨ψ] = (λ φ → ¬[φ∨ψ] (inl φ)) , (λ ψ → ¬[φ∨ψ] (inr ψ))

-- intuitionistic Excluded Middle
EM : ∀ {φ} → ¬ ¬ (φ ∨ ¬ φ)
EM ¬[φ∨¬φ] = contradiction-elim (deMorgan₁ ¬[φ∨¬φ])

private
    -- ¬¬ forms a monad on Set (only need the 'join' here though)
    ¬¬-join : {φ : Set} → ¬ ¬ ¬ ¬ φ → ¬ ¬ φ
    ¬¬-join ¬¬¬φ ¬φ = ¬¬¬φ (λ ¬¬φ → ¬¬φ ¬φ)

    -- material implication (in this form, a very interesting
    -- commentary on provability, I think)
    impl₁ : {φ ψ : Set} → ¬ (φ → ψ) → ¬ ¬ φ
    impl₁ ¬P ¬φ = ¬P (λ φ → ⊥-elim (¬φ φ))
    
    impl₂ : {φ ψ : Set} → ¬ (φ → ψ) → ¬ ψ
    impl₂ ¬P ψ = ¬P (λ _ → ψ)

    -- ¬¬ also forms an applicative functor which also has
    -- an inverse to "ap":
    ¬¬-ap⁻¹ : {φ ψ : Set} -> (¬ ¬ φ → ¬ ¬ ψ) → ¬ ¬ (φ → ψ)
    ¬¬-ap⁻¹ P ¬[φ→ψ] with impl₁ ¬[φ→ψ] | impl₂ ¬[φ→ψ] 
    ¬¬-ap⁻¹ P ¬[φ→ψ] | ¬¬φ | ¬ψ = P ¬¬φ ¬ψ

-- which is sufficient to prove that elimination of double
-- negation is possible _under_ double negation
¬¬-elim : {φ : Set} → ¬ ¬ (¬ ¬ φ → φ)
¬¬-elim = ¬¬-ap⁻¹ ¬¬-join

-- intuitionistic double-negation elimination... not sure why this form 
-- should be preferred over the that of ¬¬-elim though, other than perhaps
-- to make it seem more like the classical form (because of the classical
-- equivalence of ∄ λ x → ¬ (P x) with ∀ x P x).
NNPP : ∄ λ(φ : Set) →  ¬ (¬ ¬ φ -> φ)
NNPP (φ , ¬[¬¬φ→φ]) = ¬¬-elim {φ} ¬[¬¬φ→φ]
