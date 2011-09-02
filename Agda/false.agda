module false where

data ⊥ : Set where

relevant : .⊥ → ⊥
relevant ()

false = relevant false
