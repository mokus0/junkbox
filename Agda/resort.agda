-- Given 2 (total) orders, ≤₁ and ≤₂, and a list L sorted by ≤₁, and an 
-- arbitrary function relating lower bounds of ≤₁ to lower bounds of ≤₂
-- (I need to work out the exact relationship, but I'm pretty sure it can
-- be well-defined), implement a function that re-sorts a list ordered by
-- ≤₁ into one ordered by ≤₂ using the least possible intermediate storage
-- (and hopefully also the least possible time).
-- 
-- The algorithm is simple:
--   scan the original list, queuing items in a "live" heap ordered by _≤₂_.
--   After every new input ('x'), prune the heap by outputing (in order)
--   all elements ('y') of the heap for which (y ≤₁ f x), where 'f' is the
--   function translating bounds.
--   
-- The more tightly f is able to bound the new relation relative to the old,
-- the more the intermediate heap can be pruned and the less intermediate
-- storage is needed.  Whether this actually would translate to improved
-- performance in practice (relative to the naive approach of just sorting
-- the list from scratch, as if you knew nothing about its original structure)
-- is a much tougher question though, and is probably very dependent on the
-- "distance" between the orders.

module resort where

open import Data.AVL
open import Data.Bool
open import Data.Empty
open import Data.Product
open import Relation.Binary
  hiding (nonEmpty; module NonEmpty)
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary
open import Relation.Nullary.Decidable
open import Level

infixr 5 _∷_ _[_]∷_

-- ordered lists with known lower bound
data BoundedOrderedList {a o}{A : Set a} (_<_ : Rel A o) : A → Set (a ⊔ o) where
  []     : {x : A} → BoundedOrderedList _<_ x
  _[_]∷_ : {x : A} (y : A) → x < y → BoundedOrderedList _<_ y → BoundedOrderedList _<_ x

data OrderedList {a o}{A : Set a} (< : Rel A o) : Set (a ⊔ o) where
  [] : OrderedList <
  _∷_ : (x : A) → BoundedOrderedList < x → OrderedList <

module OrderedListProp {a o}{A : Set a}{< : Rel A o} (xs : OrderedList <) where
  null : Dec (xs ≡ [])
  null with xs
  null | []       = yes refl
  null | (x ∷ xs) = no λ()
  
  private
    -- I kinda like this; head and tail are usable, but only
    -- when it is "obvious" that xs is non-empty.
    module NonEmpty { xs≠[] : False null } where
      head : A
      head with xs | xs≠[]
      ... | []     | ()
      ... | x ∷ xs | tt = x
      
      tail : BoundedOrderedList < head
      tail with xs | xs≠[]
      ... | []     | ()
      ... | x ∷ xs | tt = xs
  
  open NonEmpty {{...}} public

forgetBound : ∀ {a o}{A : Set a}{lb : A} {< : Rel A o} → BoundedOrderedList < lb → OrderedList <
forgetBound [] = []
forgetBound (x [ _ ]∷ xs) = x ∷ xs

private
  test : ∀{a}{A : Set a} → A → A
  test {a}{A} x = head (forgetBound (tail xs))
    where
      xs : OrderedList {A = A} _≡_
      xs = x ∷ x [ refl ]∷ []
      
      open OrderedListProp
      -- odd... doesn't typecheck without something of
      -- type ⊤ in scope (comment out this import and it'll
      -- fail).  Still, it's pretty nifty that it can be
      -- made to typecheck at all.
      open import Data.Unit

module Assumptions₁ 
  {c₁ c₂ ℓ₁ ℓ₂ ℓ₃ ℓ₄}
  (decTotalOrder₁ : DecTotalOrder c₁ ℓ₁ ℓ₂)
  (strictTotalOrder₂ : StrictTotalOrder c₂ ℓ₃ ℓ₄)
  where
    open DecTotalOrder decTotalOrder₁ using ()
      renaming (Carrier to T₁; _≈_ to _≈₁_; _≤_ to _≤₁_)
    open StrictTotalOrder strictTotalOrder₂ using ()
      renaming (Carrier to T₂; _≈_ to _≈₂_; _<_ to _≤₂_)
    
    module Assumptions₂
      {ℓ₅}
      (f     : T₁ → T₂)
      (bound : T₁ → T₂ → Set ℓ₅)
      where
        heapReSort : {x : T₁}
          → Tree strictTotalOrder₂ (const T₂)
          → BoundedOrderedList _≤₁_ x
          → OrderedList _≤₂_
        heapReSort = ?
        
        resort₁ : {x : T₁} → BoundedOrderedList _≤₁_ x → BoundedOrderedList _≤₂_ ?
        resort₁ [] = []
        resort₁ {lb} (x [ lb≤₁x ]∷ xs) = ?
    
        resort : OrderedList _≤₁_ → OrderedList _≤₂_
        resort [] = []
        resort (x ∷ xs) = heapReSort (singleton (f x)) xs