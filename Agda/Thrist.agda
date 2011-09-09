{-# OPTIONS --universe-polymorphism #-}
module Thrist where

open import Data.Bool
  using (Bool; true; false)
open import Data.Maybe
open import Data.Nat
  hiding (_⊔_)
open import Data.Product
  using (∃; _×_; _,_)
open import Level
  using (_⊔_)

open import Relation.Nullary
  using (¬_; Dec; yes; no)
open import Relation.Binary.PropositionalEquality

private
  -- simplify type signatures of functions using only one kind of Thrist
  -- by globally assuming the existence of some _⇒_.  Not putting
  -- this assumption in the main module line, though, because we want
  -- it to appear explicitly in Thrist but implicitly in all the functions 
  -- below.
  module Cxt {a b}{A : Set a}(_⇒_ : A → A → Set b) where
    data Thrist : A → A → Set (a ⊔ b) where
      []  : ∀ {x} → Thrist x x
      _∷_ : ∀ {x y z} → y ⇒ z → Thrist x y → Thrist x z
    
    null : ∀{x y} → Thrist x y → Bool
    null []      = true
    null (_ ∷ _) = false
    
    [_] : ∀{x y} → x ⇒ y → Thrist x y
    [ f ] = f ∷ []
    
    _++_ : ∀{x y z} → Thrist y z → Thrist x y → Thrist x z
    []       ++ ys = ys
    (x ∷ xs) ++ ys = x ∷ (xs ++ ys)
    
    infixl 5 _∷ʳ_
    
    _∷ʳ_ : ∀ {x y z} → Thrist y z → x ⇒ y → Thrist x z
    xs ∷ʳ x = xs ++ [ x ]
    
    length : ∀{x y} → Thrist x y → ℕ
    length [] = 0
    length (_ ∷ xs) = suc (length xs)
    
    head : ∀{x z} → Thrist x z → Maybe (∃ λ y → y ⇒ z)
    head [] = nothing
    head (f ∷ _) = just (_ , f)
    
    tail : ∀{x z} → Thrist x z → Maybe (∃ λ y → x ⇒ y)
    tail [] = nothing
    tail (f ∷ []) = just (_ , f)
    tail (f ∷ fs) = tail fs
    
    foldr : ∀{c}{_↝_ : A → A → Set c}{s t}
      → (∀ {x y z} → (y ⇒ z) → (x ↝ y) → (x ↝ z))
      → (∀ {x} → x ↝ x) 
      → Thrist s t
      → s ↝ t
    foldr _∘_ id [] = id
    foldr _∘_ id (x ∷ xs) = x ∘ foldr _∘_ id xs
    
    foldMap : ∀{c}{_↝_ : A → A → Set c}{s t}
      → (∀ {x y z} → (y ↝ z) → (x ↝ y) → (x ↝ z))
      → (∀ {x} → x ↝ x) 
      → (∀ {x y} → x ⇒ y → x ↝ y)
      → Thrist s t
      → s ↝ t
    foldMap _∘_ id lift = foldr (λ f g → lift f ∘ g) id
    
    splitAt : ∀ {x z} (n : ℕ) → Thrist x z → ∃ λ y → (Thrist y z × Thrist x y)
    splitAt 0       fs  = _ , [] , fs
    splitAt (suc n) [] = _ , [] , []
    splitAt (suc n) (f ∷ fs) with splitAt n fs
    splitAt (suc n) (f ∷ fs) | (_ , gs , hs) = (_ , f ∷ gs , hs)
    
    take : ∀ {x z} (n : ℕ) → Thrist x z → ∃ λ y → Thrist y z
    take n fs with splitAt n fs
    ... | y , gs , _ = y , gs

    drop : ∀ {x z} (n : ℕ) → Thrist x z → ∃ λ y → Thrist x y
    drop n fs with splitAt n fs
    ... | y , _ , gs = y , gs
    
    span : ∀ {x z}
      → (∀ {s t} → s ⇒ t → Bool)
      → Thrist x z
      → ∃ λ y → (Thrist y z × Thrist x y)
    span p [] = _ , [] , []
    span p (f ∷ fs) with p f
    span p (f ∷ fs) | true with span p fs
    span p (f ∷ fs) | true | _ , gs , hs = _ , f ∷ gs , hs
    span p (f ∷ fs) | false = _ , [] , f ∷ fs
    
    splitOn : ∀ {p x z} → {P : ∀ y → Set p}
      → (∀ y → Dec (P y))
      → (t : Thrist x z)
      → Dec (∃ λ y → P y × ∃ λ (t₁ : Thrist y z) → ∃ λ (t₂ : Thrist x y) → t₁ ++ t₂ ≡ t)
    -- Additionally, it should be provable that every _∷_ cell in t₁ 
    -- satisfies ¬ (P z), I.E. t₁ is the shortest possible prefix satisfying
    -- the rest of the properties.  One possible way of expressing that fact:
    --   ∀ {y₂} (t₃ : Thrist y₂ z) (t₄ : Thrist y y₂) → P y₂ → t₃ ++ t₄ ≡ t₁ → T (null t₃)
    splitOn {z = z}    p fs with p z
    splitOn {z = z}    p fs | yes pz = yes (z , pz , [] , fs , refl)
    splitOn {z = z}{P} p [] | no ¬pz = no impossible
      where 
        impossible : ¬ ∃ λ y → P y × ∃ λ (t₁ : Thrist y z) → ∃ λ (t₂ : Thrist z y) → t₁ ++ t₂ ≡ []
        impossible (.z , pz , [] , _) = ¬pz pz
        impossible (y , py , _ ∷ _ , _ , ())
    splitOn {z = z} p (f ∷ fs) | no ¬pz with splitOn p fs
    splitOn {z = z} p (f ∷ .(gs ++ hs)) | no ¬pz | yes (y₂ , py₂ , gs , hs , refl)
      = yes (y₂ , py₂ , f ∷ gs , hs , refl)
    splitOn {x = x}{z}{P} p (f ∷ fs)    | no ¬pz | no ¬splitOn-p-fs 
      = no impossible
      where
        impossible : ¬ ∃ λ y₂ → P y₂ × ∃ λ (t₁ : Thrist y₂ z) → ∃ λ (t₂ : Thrist x y₂) → t₁ ++ t₂ ≡ f ∷ fs
        impossible (y₂ , py₂ , _) with f | fs | ¬splitOn-p-fs
        impossible (.z , pz  , [] , .(f ∷ fs) , refl) | f | fs | _ = ¬pz pz
        impossible (y₂ , py₂ , (.f ∷ t₁) , t₂ , refl) | f | .(t₁ ++ t₂) | ¬splitOn-p-t₁++t₂ = 
          ¬splitOn-p-t₁++t₂ (y₂ , py₂ , t₁ , t₂ , refl)

  module ICxt {a b}{A : Set a}{_⇒_ : A → A → Set b} = Cxt _⇒_

-- publish everything, making _⇒_ explicit only in cases where it does
-- not appear in a subterm of a type (i.e., only in Thrist itself).
open Cxt public
  using (Thrist)
open ICxt public
  hiding (Thrist)

map : ∀{a b c}{A : Set a}{_⇒_ : A → A → Set b}{_↝_ : A → A → Set c}
  → (∀ {x y} → x ⇒ y → x ↝ y)
  → (∀ {x y} → Thrist _⇒_ x y → Thrist _↝_ x y)
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

