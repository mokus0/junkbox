{-# OPTIONS --universe-polymorphism #-}

open import Categories.Category
open import Categories.Object.Terminal
open import Level

module terminal-exponential {o ℓ e : Level}
    (C : Category o ℓ e)
    (T : Terminal C) where

open import Categories.Object.Exponential
open import Categories.Object.Product

open Category C
open Terminal C T

import terminal-product
open terminal-product C T

⊤-exponentiable : (X : Obj) → Exponential C ⊤ X
⊤-exponentiable X = record
    { B^A       = X
    ; product   = λ{X} → [ X ×⊤]-product
    ; eval      = id
    ; λg        = λ _ g → g
    ; commutes  = λ {X}{g} → 
        begin
            id ∘ (g ∘ id)
        ↓⟨ identityˡ ⟩
            g ∘ id
        ↓⟨ identityʳ ⟩
            g
        ∎
    ; λ-unique  = λ{Y}{g}{h} h-commutes → 
        begin
            h
        ↑⟨ identityʳ ⟩
            h ∘ id
        ↑⟨ identityˡ ⟩
            id ∘ (h ∘ id)
        ↓⟨ h-commutes ⟩
            g
        ∎
    } where open HomReasoning

⊤-exponentiating : (X : Obj) → (∀ Y → Product C Y X) → Exponential C X ⊤
⊤-exponentiating X [_×X] = record
    { B^A       = ⊤
    ; product   = λ{Y} → [ Y ×X]
    ; eval      = !
    ; λg        = λ _ _ → !
    ; commutes  = !-unique₂ _ _
    ; λ-unique  = λ _ → !-unique₂ _ _
    } 
