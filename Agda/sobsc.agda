{-# OPTIONS --universe-polymorphism #-}
-- From "Sober Spaces and Continuations"
-- http://www.paultaylor.eu/ASD/sobsc/sobsc.pdf
module sobsc where

open import Algebra.Structures
    using (IsDistributiveLattice)
open import Categories.Category
open import Categories.Functor
    hiding (_≡_)
    renaming (id to idF; _∘_ to _∘F_)
open import Categories.NaturalTransformation
    hiding (_≡_)
    renaming (id to idT)
open import Categories.Monad
import Categories.Morphisms as Morphisms
open import Categories.Object.Exponential
open import Categories.Object.BinaryProducts
open import Categories.Object.Terminal
open import Categories.Object.Products
import Categories.Object.Product as Product
open import Categories.Support.EqReasoning
open import Data.Nat
    using (ℕ; zero; suc)
open import Level
import Relation.Binary.PropositionalEquality as PropEq

private
    -- Same as Data.Nat.fold but in an arbitrary universe
    ℕ-fold : ∀ {ℓ}{a : Set ℓ} → a → (a → a) → ℕ → a
    ℕ-fold z s zero    = z
    ℕ-fold z s (suc n) = s (ℕ-fold z s n)

-- TODO: F needs to be an exponential object, not a function
-- EuclideanPrinciple : ?
-- EuclideanPrinciple _≡_ Σ ⊤ = ∀ {φ F} → (φ ∧ F φ) ≡ (φ ∧ F ⊤)

-- TODO: separate proofs into IsAbstractStoneDuality
record AbstractStoneDuality {o ℓ e : Level} : Set (suc (o ⊔ ℓ ⊔ e)) where
    field
        C : Category o ℓ e
    module C = Category C
    open C hiding (op)
    open Morphisms C
    
    field
        hasProducts             : Products C
    module hasProducts = Products C hasProducts
    open Terminal       C hasProducts.terminal
    open BinaryProducts C hasProducts.binary
    
    -- useful mnemonic synonym for _⇒_; terms in context Γ.  Probably should be a bifunctor.
    Term : Obj → Obj → Set _
    Term Γ X = Γ ⇒ X
    
    -- type of global elements of objects in C (terms in the empty context)
    Elem : Obj → Set _
    Elem = Term ⊤
    
    field
        Σ                   : Obj
        Σ-exponentiating    : {A : Obj} → Exponential C A Σ
    module Σ-Power (A : Obj) = Exponential C (Σ-exponentiating {A})
    Σ↑_ : Obj → Obj
    Σ↑_ X = Σ-Power.B^A X
    
    private
        -- convert from C's chosen product to the product used in Σ-exponentiating
        Σ-Power-product : ∀ {X A} → (X × A) ⇒ Σ-Power._×A A X
        Σ-Power-product {X}{A} = Σ-prod.⟨_,_⟩ π₁ π₂
            where
                module Σ-prod = Product.Product C (Σ-Power.product A {X})

        Σ-Power-product⁻¹ : ∀ {X A} → Σ-Power._×A A X ⇒ (X × A)
        Σ-Power-product⁻¹ {X}{A} = ⟨ Σ-prod.π₁ , Σ-prod.π₂ ⟩
            where
                module Σ-prod = Product.Product C (Σ-Power.product A {X})
    
    Σ-power : ℕ → Obj → Obj
    Σ-power n X = ℕ-fold X Σ↑_ n
    
    Σ² : Obj → Obj
    Σ² = Σ-power 2
    
    -- single term which is equivalent to 'uncurry' below.
    -- There is no 'curry' equivalent though, because only Σ is required to be exponentiating.
    eval : {A : Obj} → (Σ↑ A × A) ⇒ Σ
    eval {A} = Σ-Power.eval A ∘ Σ-Power-product
    
    λ-abs : ∀ {Γ} A → Term (Γ × A) Σ → Term Γ (Σ↑ A)
    λ-abs {Γ} A f = Σ-Power.λg A Γ (f ∘ Σ-Power-product⁻¹ {Γ}{A})
    
    apply : {Γ A : Obj} → Term Γ (Σ↑ A) → Term Γ A → Term Γ Σ
    apply {A} f x = eval ∘ ⟨ f , x ⟩
    
    curry : ∀ {Γ A} → Term (Γ × A) Σ → Term Γ (Σ↑ A)
    curry = λ-abs _
    
    uncurry : ∀ {Γ A} → Term Γ (Σ↑ A) → Term (Γ × A) Σ
    uncurry f = apply (f ∘ π₁) π₂
    
    Σ↑-map : ∀ {A B} → A ⇒ B → Σ↑ B ⇒ Σ↑ A
    Σ↑-map f = curry (apply π₁ (f ∘ π₂))
    
    module Σ↑-Functor-Proofs where
        open C.HomReasoning
        open C.Equiv
        
        .Σ↑-map-id : ∀ {A} → Σ↑-map (id {A}) ≡ id {Σ↑ A}
        Σ↑-map-id {A} =
            begin
                Σ↑-map id
            ↓≣⟨ PropEq.refl ⟩
                λ-abs A (eval ∘ ⟨ π₁ , (id ∘ π₂) ⟩)
            ↓≣⟨ PropEq.refl ⟩
                λg (Σ↑ A) ((eval ∘ ⟨ π₁ , (id ∘ π₂) ⟩) ∘ Σ-Power-product⁻¹ {Σ↑ A}{A})
            ↓⟨ λ-resp-≡ lem₃ ⟩
                λg (Σ↑ A) (Σ↑A.eval ∘ Σ-prod.⟨_,_⟩ (id ∘ Σ-prod.π₁) Σ-prod.π₂)
            ↓⟨ Σ↑A.identity ⟩
                id
            ∎
            where
                module Σ↑A = Σ-Power A
                open Σ↑A using (λg; λ-resp-≡)
                module Σ-prod = Product.Product C (Σ-Power.product A {Σ↑ A})
                
                lem₁ : Σ-Power-product ∘ Σ-Power-product⁻¹ {Σ↑ A}{A} ≡ id
                lem₁ =
                    begin
                        Σ-Power-product ∘ Σ-Power-product⁻¹ {Σ↑ A}{A}
                    ↓≣⟨ PropEq.refl ⟩
                        Σ-prod.⟨_,_⟩ π₁ π₂ ∘ ⟨ Σ-prod.π₁ , Σ-prod.π₂ ⟩
                    ↑⟨ Σ-prod.g-η ⟩
                        Σ-prod.⟨_,_⟩
                            (Σ-prod.π₁ ∘ (Σ-prod.⟨_,_⟩ π₁ π₂ ∘ ⟨ Σ-prod.π₁ , Σ-prod.π₂ ⟩))
                            (Σ-prod.π₂ ∘ (Σ-prod.⟨_,_⟩ π₁ π₂ ∘ ⟨ Σ-prod.π₁ , Σ-prod.π₂ ⟩))
                    ↑⟨ Σ-prod.⟨⟩-cong₂ C.assoc C.assoc ⟩
                        Σ-prod.⟨_,_⟩
                            ((Σ-prod.π₁ ∘ Σ-prod.⟨_,_⟩ π₁ π₂) ∘ ⟨ Σ-prod.π₁ , Σ-prod.π₂ ⟩)
                            ((Σ-prod.π₂ ∘ Σ-prod.⟨_,_⟩ π₁ π₂) ∘ ⟨ Σ-prod.π₁ , Σ-prod.π₂ ⟩)
                    ↓⟨ Σ-prod.⟨⟩-cong₂ (Σ-prod.commute₁ ⟩∘⟨ refl) (Σ-prod.commute₂ ⟩∘⟨ refl) ⟩
                        Σ-prod.⟨_,_⟩
                            (π₁ ∘ ⟨ Σ-prod.π₁ , Σ-prod.π₂ ⟩)
                            (π₂ ∘ ⟨ Σ-prod.π₁ , Σ-prod.π₂ ⟩)
                    ↓⟨ Σ-prod.⟨⟩-cong₂ commute₁ commute₂ ⟩
                        Σ-prod.⟨_,_⟩ Σ-prod.π₁ Σ-prod.π₂
                    ↓⟨ Σ-prod.η ⟩
                        id
                    ∎
                
                lem₂ : Σ-prod.⟨_,_⟩ (id ∘ Σ-prod.π₁) Σ-prod.π₂ ≡ id
                lem₂ =
                    begin
                        Σ-prod.⟨_,_⟩ (id ∘ Σ-prod.π₁) Σ-prod.π₂
                    ↓⟨ Σ-prod.⟨⟩-cong₂ C.identityˡ refl ⟩
                        Σ-prod.⟨_,_⟩ Σ-prod.π₁ Σ-prod.π₂
                    ↓⟨ Σ-prod.η ⟩
                        id
                    ∎
                
                lem₃ : (eval ∘ ⟨ π₁ , (id ∘ π₂) ⟩) ∘ Σ-Power-product⁻¹ {Σ↑ A}{A}
                     ≡ Σ↑A.eval ∘ Σ-prod.⟨_,_⟩ (id ∘ Σ-prod.π₁) Σ-prod.π₂
                lem₃ = 
                    begin
                        ((Σ-Power.eval A ∘ Σ-Power-product) ∘ ⟨ π₁ , (id ∘ π₂) ⟩) ∘ Σ-Power-product⁻¹ {Σ↑ A}{A}
                    ↓⟨ (refl ⟩∘⟨ ⟨⟩-cong₂ refl C.identityˡ) ⟩∘⟨ refl ⟩
                        ((Σ-Power.eval A ∘ Σ-Power-product) ∘ ⟨ π₁ , π₂ ⟩) ∘ Σ-Power-product⁻¹ {Σ↑ A}{A}
                    ↓⟨ (refl ⟩∘⟨ η) ⟩∘⟨ refl ⟩
                        ((Σ-Power.eval A ∘ Σ-Power-product) ∘ id) ∘ Σ-Power-product⁻¹ {Σ↑ A}{A}
                    ↓⟨ C.identityʳ ⟩∘⟨ refl ⟩
                        (Σ-Power.eval A ∘ Σ-Power-product) ∘ Σ-Power-product⁻¹ {Σ↑ A}{A}
                    ↓⟨ C.assoc ⟩
                        Σ-Power.eval A ∘ (Σ-Power-product ∘ Σ-Power-product⁻¹ {Σ↑ A}{A})
                    ↓⟨ refl ⟩∘⟨ lem₁ ⟩
                        Σ-Power.eval A ∘ id
                    ↑⟨ refl ⟩∘⟨ lem₂ ⟩
                        Σ↑A.eval ∘ Σ-prod.⟨_,_⟩ (id ∘ Σ-prod.π₁) Σ-prod.π₂
                    ∎
        
        .Σ↑-map-homomorphism
            : ∀ {A B C}{f : A ⇒ B}{g : B ⇒ C}
            → Σ↑-map (g ∘ f) ≡ Σ↑-map f ∘ Σ↑-map g
        Σ↑-map-homomorphism = ?
        
        .Σ↑-map-resp-≡
            : ∀ {A B} {f g : A ⇒ B} → f ≡ g → Σ↑-map f ≡ Σ↑-map g
        Σ↑-map-resp-≡ = ?
    
    -- TODO: extract this to cover general exponentials
    Σ↑-Functor : Contravariant C C
    Σ↑-Functor = record
        { F₀            = Σ↑_
        ; F₁            = Σ↑-map
        ; identity      = Σ↑-map-id
        ; homomorphism  = Σ↑-map-homomorphism
        ; F-resp-≡      = Σ↑-map-resp-≡
        } where open Σ↑-Functor-Proofs
    module Σ↑-Functor = Functor Σ↑-Functor
    
    Σ²-Functor : Endofunctor C
    Σ²-Functor = Σ↑-Functor ∘F Σ↑-Functor.op
    module Σ²-Functor = Functor Σ²-Functor
    
