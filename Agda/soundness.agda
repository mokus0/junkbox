{-# OPTIONS --universe-polymorphism #-}
module soundness where

open import Level

data ⊥ {n} : Set n where

¬_ : ∀ {n} → Set n → Set n
¬_ {n} x = x → ⊥ {n}

-- Proof (in Set (suc n)) that Set n is sound ;)
sound : ∀ n → ¬ ⊥ {n}
sound n ()
