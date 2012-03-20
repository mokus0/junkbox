{-# OPTIONS --universe-polymorphism #-}
module Algebra.DirectedSet where

open import Algebra.FunctionProperties
    using (Op₁; Op₂; _DistributesOverʳ_)
open import Relation.Binary
    using (Rel; IsPreorder)
open import Level

record IsDirectedSet
  {a ℓ₁ ℓ₂} {A : Set a}
  (_≈_ : Rel A ℓ₁) -- The underlying equality.
  (_≤_ : Rel A ℓ₂) -- The relation.
  (_∨_ : Op₂ A)    -- least upper bound
  : Set (a ⊔ ℓ₁ ⊔ ℓ₂) where
  field
    isPreorder : IsPreorder _≈_ _≤_
    ∨-bound-l : ∀ {φ ψ} → φ ≤ (φ ∨ ψ)
    ∨-bound-r : ∀ {φ ψ} → ψ ≤ (φ ∨ ψ)

record DirectedSet (c ℓ₁ ℓ₂ : Level) : Set (suc (c ⊔ ℓ₁ ⊔ ℓ₂)) where
    field
        Carrier : Set c
        _≈_ : Rel Carrier ℓ₁  -- The underlying equality.
        _≤_ : Rel Carrier ℓ₂  -- The relation.
        _∨_ : Op₂ Carrier     -- least upper bound
        isDirectedSet : IsDirectedSet _≈_ _≤_ _∨_
    open IsDirectedSet isDirectedSet public

