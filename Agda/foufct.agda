{-# OPTIONS --universe-polymorphism #-}
-- from "Foundations for Computable Topology §4.2-4.3
-- (http://www.paultaylor.eu/ASD/foufct.pdf)
-- 
-- Not sure whether I read it right, though.  The monad
-- came out as a comonad.
module foufct where

open import Categories.Adjunction
    using (Adjunction; module Adjunction)
open import Categories.Category
    using (Category; module Category)
open import Categories.Functor
    using (Functor; module Functor)
    renaming (id to idF; _∘_ to _∘F_)
open import Categories.Morphisms
    using (Iso)
open import Categories.NaturalTransformation
    using (NaturalTransformation; module NaturalTransformation)
open import Data.Product
    using (∃)
open import Level
    using (Level; suc; _⊔_)

-- T gives the algebra associated with a topological space
-- P gives the space of primes generated by an algebra
-- This is the definition in the paper, but it is weird...
-- it leads to monads in the _opposite_ categories A.op and 
-- S.op, not in A and S as the text implies.
record AbstractStoneDuality
    {o ℓ e o₁ ℓ₁ e₁}
    (S : Category o ℓ e)
    (A : Category o₁ ℓ₁ e₁)
    : Set (o ⊔ ℓ ⊔ e ⊔ o₁ ⊔ ℓ₁ ⊔ e₁) where
    private module S = Category S
    private module A = Category A
    
    field
        P : Functor A.op S
        T : Functor S A.op
        duality : Adjunction P T
    
    private module P = Functor P
    private module T = Functor T
    private module duality = Adjunction duality
    
    iota : NaturalTransformation {C = A.op}{D = A.op} idF (T ∘F P)
    iota = duality.unit
    
    eta : NaturalTransformation {C = S.op}{D = S.op} idF (P.op ∘F T.op)
    eta = NaturalTransformation.op duality.counit
    
    -- type of sober spaces
    IsSober : S.Obj → Set (o ⊔ ℓ ⊔ e)
    IsSober X = ∃ (Iso S (NaturalTransformation.η eta X))
    
    -- type of spatial algebras
    IsSpatial : A.Obj → Set (o₁ ⊔ ℓ₁ ⊔ e₁)
    IsSpatial X = ∃ (Iso A (NaturalTransformation.η iota X))
