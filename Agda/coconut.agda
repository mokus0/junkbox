module coconut where

open import Data.Nat
open import Data.Fin as Fin using (Fin; inject₁)
open import Data.Vec using (Vec; _∷_; []; lookup; init; last; foldr)
open import Data.Product
open import Relation.Binary using (module DecTotalOrder)
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary using (¬_)

sum : ∀ {n} → Vec ℕ n → ℕ
sum = foldr (λ _ → ℕ) _+_ 0

{-
  A classic puzzle:  There are 5 men and a monkey, shipwrecked on an island.
  The men gather up a pile of coconuts, intending to divide them evenly, but
  it gets late and they go to sleep before dividing them up.  One by one,
  each of the men wakes up in the middle of the night and decides to take his
  share.  Each time, he finds that there is 1 too many coconuts to divide them
  evenly, so he gives one to the monkey and takes his share.
  
  In the morning, everyone wakes up and they divide the coconuts up evenly,
  again finding there is one too many to divide them evenly (so the monkey
  gets one more).
  
  What is the least possible number of coconuts they originally collected?
  
  Your task, should you choose to accept it, is to not only discover the
  answer but prove your answer correct.  Bonus points for replacing '5' with
  a variable.
 -}

{-
  Step 1: name the numbers and write out the constraints.
    - I'll call the number of men (5) 'n'.
    - I'll call the number of coconut at the start 'c'.
    - I'll number the men by 'i', where 0 <= i < n, ordered such that i < j
      iff i woke up before j.
    - I'll call the number of coconuts taken by man 'i' 'x_{i}'.
    - I'll call the number of coconuts each man gets in the last division
      'x_n'.
  -}
  {-  So, we have the following variables:
        n : ℕ, known
        c : ℕ, unknown
        x_{i} : ℕ, unknown, where i : Fin (n + 1)
   -}
    
module constraints (n : ℕ) (c : ℕ) (xs : Vec ℕ (1 + n)) where
   {- And we have these constraints:
        A) n * x_0 + 1 = c
        B) ∀ i (0 <= i < n)  → 
            n * x_{i+1} + 1 = (n - 1) * x_i
        C) (Σ (0 <= i < n) x_i + 1) + n * x_n + 1 = c
   -}
  A = 1 + n * lookup Fin.zero xs ≡ c
  B = ∀ (i : Fin n) → n * lookup (Fin.suc i) xs + 1 + lookup (inject₁ i) xs ≡ n * lookup (inject₁ i) xs
  C = sum (init xs) + n + (n * last xs) + 1 ≡ c

  -- convenience for shortening the definition of the minimization property:
  allConstraints = A × B × C

record solution (n : ℕ) : Set where
  open constraints n
  
  field
    c : ℕ
    xs : Vec ℕ (1 + n)
  
    valid   : allConstraints c xs
    minimal : ∀ k → k < c → ¬ ∃ (allConstraints k)

s5 : solution 5
s5 = record
  { c = c; xs = xs
  ; valid = refl , f , refl
  ; minimal = minimal
  }
  where
    c = 15621
    xs = 3124 ∷ 2499 ∷ 1999 ∷ 1599 ∷ 1279 ∷ 1023 ∷ []
    
    f : ∀ i → 5 * lookup (Fin.suc i) xs + 1 + lookup (inject₁ i) xs ≡ 5 * lookup (inject₁ i) xs
    f Fin.zero                                         = refl
    f (Fin.suc Fin.zero)                               = refl
    f (Fin.suc (Fin.suc Fin.zero))                     = refl
    f (Fin.suc (Fin.suc (Fin.suc Fin.zero)))           = refl
    f (Fin.suc (Fin.suc (Fin.suc (Fin.suc Fin.zero)))) = refl
    f (Fin.suc (Fin.suc (Fin.suc (Fin.suc (Fin.suc ())))))
    
    open constraints 5
    minimal : ∀ k → k < c → ¬ ∃ λ ys -> (A k ys × B k ys × C k ys)
    minimal k k<c (a , b , c) = ?
      --  context:
      --    k : ℕ
      --    k<c : k < 15621
      --    a : 5 * y_0 + 1 = k
      --    b i : 5 * y_{i+1} + 1 + y_i ≡ 5 * y_i
      --    c : sum (init ys) + 5 + 5 * last ys + 1 ≡ k
      --  outline of proof:
      --    prove that k ≡ -4 mod 5^6
      --    observe that c ≡ -4 mod 5^6
      --    observe that these 2 facts are incompatible with the assumption
      --      that 0 <= k < c < 5^6
