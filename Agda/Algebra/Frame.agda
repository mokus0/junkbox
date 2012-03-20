{-# OPTIONS --universe-polymorphism #-}
module Algebra.Frame where

open import Algebra.DirectedSet
open import Algebra.CompleteLattice

open import Algebra.Structures
    using (IsDistributiveLattice; module IsDistributiveLattice)
open import Algebra
    using (DistributiveLattice)
open import Algebra.FunctionProperties
    using (Op₂; _DistributesOverʳ_)

open import Data.Bool
    using (Bool; if_then_else_)

open import Level

open import Relation.Binary
    using (Rel)

record IsFrame
    {a ℓ₁ ℓ₂ i iℓ₁ iℓ₂} {A : Set a}
    (_≈_ : Rel A ℓ₁)
    (_≤_ : Rel A ℓ₂)
    (_∨_ _∧_ : Op₂ A)
    -- is this a reasonable defn of a directed join?  would it be a reasonable
    -- defn of 'sup' in IsCompleteLattice (probably not)?  Does the "obvious"
    -- dual make sense as a directed meet?  
    (∃ : {I : DirectedSet i iℓ₁ iℓ₂} → (DirectedSet.Carrier I → A) → A)
    : Set (a ⊔ ℓ₁ ⊔ ℓ₂ ⊔ suc (i ⊔ iℓ₁ ⊔ iℓ₂)) where
    
    field
        isCompleteLattice     : IsCompleteLattice _≈_ _≤_ _∨_ _∧_
        isDistributiveLattice : IsDistributiveLattice _≈_ _∨_ _∧_
        ∃-∧-distrib
            : {I : DirectedSet i iℓ₁ iℓ₂}
            → ∀{φ : A}{P : DirectedSet.Carrier I → A}
            → (∃ {I = I} λ ψ → φ ∧ P ψ) ≈ (φ ∧ ∃ {I = I} P)
    module Eq  = IsCompleteLattice.Eq  isCompleteLattice
    module Ord = IsCompleteLattice.Ord isCompleteLattice
    open IsDistributiveLattice isDistributiveLattice public

-- record Frame (c ℓ i iℓ₁ iℓ₂ : Level) : Set (suc (c ⊔ ℓ ⊔ suc (i ⊔ iℓ₁ ⊔ iℓ₂))) where
--     field
--         Carrier : DistributiveLattice c
--         (_≈_ : Rel A ℓ)
--         (_∨_ _∧_ : Op₂ A)
--         (∃ : {I : DirectedSet i iℓ₁ iℓ₂} → (DirectedSet.Carrier I → A) → A)
--         isFrame : 
