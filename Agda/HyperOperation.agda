module HyperOperation where

open import Algebra.Structures
open import Data.Nat
open import Data.Nat.Properties
open import Data.Sum
open import Data.Product using (_×_; _,_)
open import Data.Unit using (⊤)
open import Relation.Binary using (module DecTotalOrder)
open import Relation.Binary.PropositionalEquality

-- Goodstein's Hyperoperation, reference implementation, directly
-- transcribed from http://en.wikipedia.org/wiki/Hyperoperation,
-- revision from 29 August 2011 at 17:34.
H-ref : ℕ → ℕ → ℕ → ℕ
H-ref 0 _ b = suc b
H-ref 1 a 0 = a
H-ref 2 _ 0 = 0
H-ref _ _ 0 = 1
H-ref (suc n) a (suc b) = H-ref n a (H-ref (suc n) a b)

-- optimized a bit (equivalence proof below)
H : ℕ → ℕ → ℕ → ℕ
H 0 a b = suc b
H 1 a b = a + b
H 2 a b = a * b
H n a 0 = 1
H (suc n) a (suc b) = H n a (H (suc n) a b)

-- Knuth's "up arrow" notation
_↑[_]_ : ℕ → ℕ → ℕ → ℕ
a ↑[ n ] b = H (suc (suc n)) a b

module H-Equiv where
  open IsCommutativeSemiring isCommutativeSemiring
    using (+-isCommutativeMonoid; *-isCommutativeMonoid)
  open IsCommutativeMonoid +-isCommutativeMonoid
    using ()
    renaming (comm to +-comm; identityˡ to +-identityˡ)
  
  +-identityʳ : ∀ a → a + 0 ≡ a
  +-identityʳ a = trans (+-comm a 0) (+-identityˡ a)
  
  open IsCommutativeMonoid *-isCommutativeMonoid
    using ()
    renaming (comm to *-comm)
  
  lem₀ : ∀ a b → suc (a + b) ≡ a + suc b
  lem₀ 0  b = refl
  lem₀ (suc a) b rewrite lem₀ a b = refl
  
  H₀≡H₀-ref : ∀ a b → H-ref 0 a b ≡ H 0 a b
  H₀≡H₀-ref a b = refl
  
  H₁≡H₁-ref : ∀ a b → H-ref 1 a b ≡ H 1 a b
  H₁≡H₁-ref a 0 = sym (+-identityʳ a)
  H₁≡H₁-ref a (suc b)
    rewrite H₁≡H₁-ref a b
      = lem₀ a b
  
  H₂≡H₂-ref : ∀ a b → H-ref 2 a b ≡ H 2 a b
  H₂≡H₂-ref a 0 rewrite *-comm a 0 = refl
  H₂≡H₂-ref a (suc b)
    rewrite H₂≡H₂-ref a b
          | H₁≡H₁-ref a (a * b)
          | *-comm a b
          | *-comm a (suc b)
      = refl
  
  H≡H-ref′ : ∀ n a b → H-ref (suc (suc n)) a b ≡ H (suc (suc n)) a b
  H≡H-ref′ 0       a b = H₂≡H₂-ref a b
  H≡H-ref′ (suc n) a 0 = refl
  H≡H-ref′ (suc n) a (suc b)
    rewrite H≡H-ref′ (suc n) a b
          | H≡H-ref′ n a (H (suc (suc (suc n))) a b)
      = refl
  
  -- proof that the optimization is sound
  H≡H-ref : ∀ n a b → H-ref n a b ≡ H n a b
  H≡H-ref 0 a b = H₀≡H₀-ref a b
  H≡H-ref 1 a b = H₁≡H₁-ref a b
  H≡H-ref (suc (suc n)) a b = H≡H-ref′ n a b

-- Properties of H
open H-Equiv using (H≡H-ref)

-- Why can't this whole thing be just one refl?
-- The LHS only matches in one case...
H-ref-step : ∀ n a b → H-ref (suc n) a (suc b) ≡ H-ref n a (H-ref (suc n) a b)
H-ref-step 0 a b = refl
H-ref-step 1 a b = refl
H-ref-step (suc (suc n)) a b = refl

H-step : ∀ n a b → H (suc n) a (suc b) ≡ H n a (H (suc n) a b)
H-step n a b
  rewrite sym (H≡H-ref (suc n) a (suc b))
        | sym (H≡H-ref n a (H (suc n) a b))
        | sym (H≡H-ref (suc n) a b)
    = H-ref-step n a b

mutual
  qua : ∀ n a → H-ref (suc n) a (H-ref (suc (suc n)) a 0) ≡ a
  qua 0 a = refl
  qua (suc n) a rewrite eep n a = refl

  eep : ∀ n a → H-ref (suc (suc n)) a 1 ≡ a
  eep n a = 
    begin
      H-ref (suc (suc n)) a (suc 0)
    ≡⟨ H-ref-step (suc n) a 0 ⟩
      H-ref (suc n) a (H-ref (suc (suc n)) a 0)
    ≡⟨ qua n a ⟩
      a
    ∎
    where
      open ≡-Reasoning

foo : ∀ n a → H-ref (suc (suc n)) a 2 ≡ H-ref (suc n) a a
foo n a = 
  begin
    H-ref (suc (suc n)) a 2
  ≡⟨ H-ref-step (suc n) a 1 ⟩ -- Why can't this be refl?
    H-ref (suc n) a (H-ref (suc (suc n)) a 1)
  ≡⟨ cong (H-ref (suc n) a) (eep n a) ⟩
    H-ref (suc n) a a
  ∎
  where
    open ≡-Reasoning

bar : ∀ n → H-ref (suc n) 2 2 ≡ 4
bar zero = refl
bar (suc n) rewrite foo n 2 = bar n

module H-compare where
  open ≤-Reasoning
  open DecTotalOrder decTotalOrder
    using () 
    renaming 
      ( refl to ≤-refl
      ; trans to ≤-trans
      )
  
  open IsCommutativeSemiring isCommutativeSemiring
    using (+-isCommutativeMonoid; +-cong; +-comm)
  open IsCommutativeMonoid +-isCommutativeMonoid
    using () renaming (identityˡ to +-identityˡ)
  
  +-identityʳ : ∀ a → a + 0 ≡ a
  +-identityʳ a = trans (+-comm a 0) (+-identityˡ a)
  
  H-compare : ∀ n₁ n₂ a₁ a₂ b₁ b₂ → Ordering (H n₁ a₁ b₁) (H n₂ a₂ b₂)
  H-compare n₁ n₂ a₁ a₂ b₁ b₂ =
    compare (H n₁ a₁ b₁) (H n₂ a₂ b₂)
  

-- module H-mono where
--   open ≤-Reasoning
--   open DecTotalOrder decTotalOrder
--     using () 
--     renaming 
--       ( refl to ≤-refl
--       ; trans to ≤-trans
--       )
--   
--   open IsCommutativeSemiring isCommutativeSemiring
--     using (+-isCommutativeMonoid; +-cong; +-comm)
--   open IsCommutativeMonoid +-isCommutativeMonoid
--     using () renaming (identityˡ to +-identityˡ)
--   
--   +-identityʳ : ∀ a → a + 0 ≡ a
--   +-identityʳ a = trans (+-comm a 0) (+-identityˡ a)
--   
--   private
--     lem₁ : ∀ {n a}
--       → suc n ≤ 2 ⊎ 1 ≤ a
--       → n ≤ 2 ⊎ 1 ≤ a
--     lem₁ (inj₁ (s≤s n≤1)) = inj₁ (≤-trans n≤1 (s≤s (z≤n {1})))
--     lem₁ (inj₂ 1≤a)       = inj₂ 1≤a
--   
--   H-mono₁-precondition : ℕ → ℕ → ℕ → ℕ → Set
--   H-mono₁-precondition 0  1  a b = 1 ≤ a
--   H-mono₁-precondition 0  2  a b = 2 ≤ a × 1 ≤ b
--   H-mono₁-precondition 0  n  a b = 2 ≤ a
--   H-mono₁-precondition 1  2  a b = a + b ≤ a * b
--   H-mono₁-precondition n₁ n₂ a b = 1 ≤ a
--   
--   mutual
--     H-mono₁ : ∀ {n₁ n₂}
--       → n₁ ≤ n₂
--       → ∀ a b
--       → H-mono₁-precondition n₁ n₂ a b
--       → H n₁ a b ≤ H n₂ a b
--     H-mono₁ {0}{0} _ a b _   = ≤-refl
--     H-mono₁ {0}{1} _ a b 1≤a = 1≤a +-mono (≤-refl {x = b})
--     H-mono₁ {0}{2} _ a b (2≤a , 1≤b) =
--       begin
--         1 + b
--       ≤⟨ 1≤b +-mono (≤-refl {x = b}) ⟩
--         b + b
--       ≡⟨ sym (+-cong (refl {x = b}) (+-identityʳ b)) ⟩
--         b + (b + 0)
--       ≤⟨ 2≤a *-mono (≤-refl {x = b}) ⟩
--         a * b
--       ∎
--     H-mono₁ {0}{suc (suc (suc n₂))} _ a 0 _ = ≤-refl
--     H-mono₁ {0}{suc (suc (suc n₂))} _ a (suc b) 2≤a =
--       begin
--         H 0 a (suc b)
--       ≤⟨ H-mono₁ (z≤n {suc (suc n₂)}) a (suc b) precond ⟩ -- precond = case n₂ of 0 → ; _ → 
--         H (suc (suc n₂)) a (suc b)
--       ≤⟨ H-mono₃ (suc (suc n₂)) a (H-mono₁ (z≤n {suc (suc (suc n₂))}) a b 2≤a) (inj₂ 1≤a) ⟩
--         H (suc (suc n₂)) a (H (suc (suc (suc n₂))) a b)
--       ≡⟨ sym (H-step (suc (suc n₂)) a b) ⟩
--         H (suc (suc (suc n₂))) a (suc b)
--       ∎
--       where
--         1≤a : 1 ≤ a
--         1≤a = ≤-trans (s≤s z≤n) 2≤a
--         
--         precond : H-mono₁-precondition 0 (suc (suc n₂)) a (suc b)
--         precond with n₂
--         precond | zero  = (2≤a , s≤s (z≤n {b}))
--         precond | suc _ = 2≤a
--     
--     H-mono₁ {suc n₁}{0} () _ _ _
--     H-mono₁ {1}{1} _ a b _ = ≤-refl
--     H-mono₁ {1}{2} _ a b p = p
--     H-mono₁ {1}{suc (suc (suc n₂))} n₁≤n₂ a b _ =
--       begin
--         H 1 a b -- a + b
--       ≤⟨ ? ⟩
--         H (suc (suc (suc n₂))) a b
--       ∎
--     
--     H-mono₁ {suc (suc n₁)}{1} (s≤s ()) _ _ _
--     H-mono₁ {2}{2} n₁≤n₂ a b _ = ≤-refl
--     H-mono₁ {2}{suc (suc (suc n₂))} n₁≤n₂ a b _ =
--       begin
--         H 2 a b -- a * b
--       ≤⟨ ? ⟩
--         H (suc (suc (suc n₂))) a b
--       ∎
--     
--     H-mono₁ {suc (suc (suc n₁))}{2} (s≤s (s≤s ())) _ _ _
--     H-mono₁ {suc (suc (suc n₁))}{(suc (suc (suc n₂)))} (s≤s (s≤s (s≤s n₁≤n₂))) a 0 _ = ≤-refl
--     H-mono₁ {suc (suc (suc n₁))}{(suc (suc (suc n₂)))} (s≤s (s≤s (s≤s n₁≤n₂))) a (suc b) 1≤a =
--       begin
--         H (suc (suc (suc n₁))) a (suc b)
--       ≡⟨ H-step (suc (suc n₁)) a b ⟩
--         H (suc (suc n₁)) a (H (suc (suc (suc n₁))) a b)
--       ≤⟨ H-mono₁ (s≤s (s≤s n₁≤n₂)) a (H (suc (suc (suc n₁))) a b) 1≤a ⟩
--         H (suc (suc n₂)) a (H (suc (suc (suc n₁))) a b)
--       ≤⟨ H-mono₃ (suc (suc n₂)) a (H-mono₁ (s≤s (s≤s (s≤s n₁≤n₂))) a b 1≤a) (inj₂ 1≤a) ⟩
--         H (suc (suc n₂)) a (H (suc (suc (suc n₂))) a b)
--       ≡⟨ sym (H-step (suc (suc n₂)) a b) ⟩
--         H (suc (suc (suc n₂))) a (suc b)
--       ∎
-- 
--     H-mono₂ : ∀ n {a₁ a₂}
--       → a₁ ≤ a₂
--       → ∀ b
--       → n ≤ 2 ⊎ 1 ≤ a₂
--       → H n a₁ b ≤ H n a₂ b
--     H-mono₂ 0       {a₁}{a₂} a₁≤a₂ b _ = ≤-refl
--     H-mono₂ 1 {a₁}{a₂} a₁≤a₂ zero _ = a₁≤a₂ +-mono (z≤n {0})
--     H-mono₂ 2 {a₁}{a₂} a₁≤a₂ zero _ = a₁≤a₂ *-mono (z≤n {0})
--     H-mono₂ (suc (suc (suc n))) {a₁}{a₂} a₁≤a₂ zero _ = ≤-refl
--     H-mono₂ (suc n) {a₁}{a₂} a₁≤a₂ (suc b) exc =
--       begin
--           H (suc n) a₁ (suc b)
--       ≡⟨ H-step n a₁ b ⟩
--           H n a₁ (H (suc n) a₁ b)
--       ≤⟨ H-mono₂ n a₁≤a₂ (H (suc n) a₁ b) (lem₁ exc) ⟩
--           H n a₂ (H (suc n) a₁ b)
--       ≤⟨ H-mono₃ n a₂ (H-mono₂ (suc n) a₁≤a₂ b exc) (lem₁ exc) ⟩
--           H n a₂ (H (suc n) a₂ b)
--       ≡⟨ sym (H-step n a₂ b) ⟩
--           H (suc n) a₂ (suc b)
--       ∎
--     
--     H-mono₃ : ∀ n a {b₁ b₂}
--       → b₁ ≤ b₂
--       → n ≤ 2 ⊎ 1 ≤ a
--       → H n a b₁ ≤ H n a b₂
--     H-mono₃   zero  a {b₁}{b₂} b₁≤b₂ _ = s≤s b₁≤b₂
--     H-mono₃ (suc n) a { zero }{ zero } z≤n _ = ≤-refl
--     H-mono₃ (suc n) a {suc b₁}{ zero } () _
--     H-mono₃ 1 a { zero }{suc b₂} z≤n _ = (≤-refl {x = a}) +-mono (z≤n {suc b₂})
--     H-mono₃ 2 a { zero }{suc b₂} z≤n _ = (≤-refl {x = a}) *-mono (z≤n {suc b₂})
--     H-mono₃ (suc (suc (suc n))) a { zero }{suc b₂} z≤n (inj₁ (s≤s (s≤s ())))
--     H-mono₃ (suc (suc (suc n))) a { zero }{suc b₂} z≤n (inj₂ 1≤a) =
--       begin
--         H (suc (suc (suc n))) a 0
--       ≡⟨ refl ⟩
--         1
--       ≤⟨ 1≤a *-mono (≤-refl {x = 1}) ⟩
--         a * 1
--       ≤⟨ H-mono₃ 2 a (H-mono₃ 3 a (z≤n {b₂}) (inj₂ 1≤a)) (inj₂ 1≤a) ⟩
--         H 2 a (H 3 a b₂)
--       ≡⟨ sym (H-step 2 a b₂) ⟩
--         H 3 a (suc b₂)
--       ≤⟨ H-mono₁ (s≤s (s≤s (s≤s (z≤n {n})))) a (suc b₂) 1≤a ⟩
--         H (suc (suc (suc n))) a (suc b₂)
--       ∎
--     H-mono₃ (suc n) a {suc b₁}{suc b₂} (s≤s b₁≤b₂) exc =
--       begin
--         H (suc n) a (suc b₁)
--       ≡⟨ H-step n a b₁ ⟩
--         H n a (H (suc n) a b₁)
--       ≤⟨ H-mono₃ n a (H-mono₃ (suc n) a b₁≤b₂ exc) (lem₁ exc) ⟩
--         H n a (H (suc n) a b₂)
--       ≡⟨ sym (H-step n a b₂) ⟩
--         H (suc n) a (suc b₂)
--       ∎
    
