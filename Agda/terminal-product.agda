{-# OPTIONS --universe-polymorphism #-}

open import Categories.Category
open import Categories.Object.Terminal
open import Level

module terminal-product {o ℓ e : Level}
    (C : Category o ℓ e)
    (T : Terminal C) where

open import Categories.Object.Product

open Category C
open Terminal C T

[⊤×_] : Obj → Obj
[⊤× X ] = X

[⊤×_]-product : (X : Obj) → Product C ⊤ X
[⊤×_]-product X = record
    { A×B       = [⊤× X ]
    ; π₁        = !
    ; π₂        = id
    ; ⟨_,_⟩     = λ _ f → f
    ; commute₁  = !-unique₂ _ _
    ; commute₂  = identityˡ
    ; universal = λ {A}{f}{g}{i} _ id∘i≡g → 
        begin
            g
        ↑⟨ id∘i≡g ⟩ 
            id ∘ i
        ↓⟨ identityˡ ⟩
            i
        ∎
    } where open HomReasoning

[_×⊤] : Obj → Obj
[ X ×⊤] = X

[_×⊤]-product : (X : Obj) → Product C X ⊤
[_×⊤]-product X = record
    { A×B       = [ X ×⊤]
    ; π₁        = id
    ; π₂        = !
    ; ⟨_,_⟩     = λ f _ → f
    ; commute₁  = identityˡ
    ; commute₂  = !-unique₂ _ _
    ; universal = λ {A}{f}{g}{i} id∘i≡f _ → 
        begin
            f
        ↑⟨ id∘i≡f ⟩ 
            id ∘ i
        ↓⟨ identityˡ ⟩
            i
        ∎
    } where open HomReasoning

