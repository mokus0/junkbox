{-# OPTIONS --universe-polymorphism #-}
module Algebra.CompleteLattice where

open import Algebra.Structures
    using (IsLattice; module IsLattice)
open import Algebra.FunctionProperties
    using (Op₂)
open import Level
open import Relation.Binary
    using (Rel; IsPartialOrder; module IsPartialOrder)

-- todo: how to define subsets? (ie, what should be the type of 'sup' and 'inf'?)
record IsCompleteLattice
    {a ℓ₁ ℓ₂}
    {A : Set a}
    (≈ : Rel A ℓ₁)
    (≤ : Rel A ℓ₂)
    (∨ ∧ : Op₂ A)
    : Set (a ⊔ ℓ₁ ⊔ ℓ₂) where
    field
        isPartialOrder  : IsPartialOrder ≈ ≤
        isLattice       : IsLattice ≈ ∨ ∧
    module Ord = IsPartialOrder isPartialOrder
    open Ord public
        renaming (refl      to ≤-refl; 
                  reflexive to ≤-reflexive;
                  trans     to ≤-trans)
    open IsLattice      isLattice public
        hiding (isEquivalence)
        renaming (refl      to ≈-refl; 
                  reflexive to ≈-reflexive;
                  trans     to ≈-trans)
