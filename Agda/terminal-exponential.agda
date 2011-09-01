{-# OPTIONS --universe-polymorphism #-}

open import Categories.Category
open import Categories.Object.Terminal
open import Level

module terminal-exponential {o ℓ e : Level}
    (C : Category o ℓ e)
    (T : Terminal C) where

open Category C
open Terminal C T

import Categories.Object.Exponential
import Categories.Object.Product
import Categories.Object.Product.Morphisms

open Categories.Object.Exponential C hiding (convert)
open Categories.Object.Product C
open Categories.Object.Product.Morphisms C

import terminal-product
open terminal-product C T

⊤-exponentiable : (B : Obj) → Exponential ⊤ B
⊤-exponentiable B = record
    { B^A       = B
    ; product   = [ B ×⊤]-product
    ; eval      = id
    ; λg        = λ {X} p g → g ∘ convert [ X ×⊤]-product p
    ; β         = λ {X} p {g} → 
        begin
            id ∘ (g ∘ [ p ]⟨ id , ! ⟩) ∘ [ p ]π₁
        ↓⟨ identityˡ ⟩
            (g ∘ [ p ]⟨ id , ! ⟩) ∘ [ p ]π₁
        ↓⟨ assoc ⟩
            g ∘ [ p ]⟨ id , ! ⟩ ∘ [ p ]π₁
        ↓⟨ refl ⟩∘⟨ Product.⟨⟩-distrib p ⟩
            g ∘ [ p ]⟨ id ∘ [ p ]π₁ , ! ∘ [ p ]π₁ ⟩
        ↓⟨ refl ⟩∘⟨ Product.⟨⟩-cong₂ p identityˡ (!-unique₂ (! ∘ [ p ]π₁) [ p ]π₂) ⟩
            g ∘ [ p ]⟨ [ p ]π₁ , [ p ]π₂ ⟩
        ↓⟨ refl ⟩∘⟨ Product.η p ⟩
            g ∘ id
        ↓⟨ identityʳ ⟩
            g
        ∎
    ; λ-unique  = λ{X} p {g}{h} h-commutes → 
        begin
            h
        ↑⟨ identityʳ ⟩
            h ∘ id
        ↑⟨ refl ⟩∘⟨ Product.commute₁ p ⟩
            h ∘ [ p ]π₁ ∘ [ p ]⟨ id , ! ⟩
        ↑⟨ assoc ⟩
            (h ∘ [ p ]π₁) ∘ [ p ]⟨ id , ! ⟩
        ↑⟨ identityˡ ⟩∘⟨ refl ⟩
            (id ∘ h ∘ [ p ]π₁) ∘ [ p ]⟨ id , ! ⟩
        ↓⟨ h-commutes ⟩∘⟨ refl ⟩
            g ∘ convert [ X ×⊤]-product p
        ∎
    } 
    where
    open HomReasoning
    open Equiv

⊤-exponentiating : (A : Obj) → Exponential A ⊤
⊤-exponentiating A = record
    { B^A       = ⊤
    ; product   = [⊤× A ]-product
    ; eval      = !
    ; λg        = λ _ _ → !
    ; β         = λ _   → !-unique₂ _ _
    ; λ-unique  = λ _ _ → !-unique₂ _ _
    } 
