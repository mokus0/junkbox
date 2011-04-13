------------------------------------------------------------------------
-- Various forms of induction for natural numbers
------------------------------------------------------------------------

module Induction.Nat where

open import Function
open import Data.Nat
open import Data.Fin
open import Data.Fin.Props
open import Data.Product
open import Data.Unit
open import Induction
open import Induction.WellFounded as WF
open import Relation.Binary.PropositionalEquality
open import Relation.Unary

------------------------------------------------------------------------
-- Ordinary induction

Rec : RecStruct ℕ
Rec P zero    = ⊤
Rec P (suc n) = P n

rec-builder : RecursorBuilder Rec
rec-builder P f zero    = tt
rec-builder P f (suc n) = f n (rec-builder P f n)

rec : Recursor Rec
rec = build rec-builder

------------------------------------------------------------------------
-- Complete induction

CRec : RecStruct ℕ
CRec P zero    = ⊤
CRec P (suc n) = P n × CRec P n

cRec-builder : RecursorBuilder CRec
cRec-builder P f zero    = tt
cRec-builder P f (suc n) = f n ih , ih
  where ih = cRec-builder P f n

cRec : Recursor CRec
cRec = build cRec-builder

------------------------------------------------------------------------
-- Complete induction based on _<′_

<-Rec : RecStruct ℕ
<-Rec = WfRec _<′_

mutual

  <-well-founded : Well-founded _<′_
  <-well-founded n = acc (<-well-founded′ n)

  <-well-founded′ : ∀ n → <-Rec (Acc _<′_) n
  <-well-founded′ zero     _ ()
  <-well-founded′ (suc n) .n ≤′-refl       = <-well-founded n
  <-well-founded′ (suc n)  m (≤′-step m<n) = <-well-founded′ n m m<n

open WF.All <-well-founded public
  renaming ( wfRec-builder to <-rec-builder
           ; wfRec to <-rec
           )

------------------------------------------------------------------------
-- Complete induction based on _≺_

≺-Rec : RecStruct ℕ
≺-Rec = WfRec _≺_

≺-well-founded : Well-founded _≺_
≺-well-founded = Subrelation.well-founded ≺⇒<′ <-well-founded

open WF.All ≺-well-founded public
  renaming ( wfRec-builder to ≺-rec-builder
           ; wfRec to ≺-rec
           )

------------------------------------------------------------------------
-- Examples

private

 module Examples where

  -- The half function.

  HalfPred : ℕ → Set
  HalfPred _ = ℕ

  half₁ : ℕ → ℕ
  half₁ = cRec HalfPred half₁'
    where
    half₁' : ∀ n → CRec HalfPred n → HalfPred n
    half₁' zero          _                = zero
    half₁' (suc zero)    _                = zero
    half₁' (suc (suc n)) (_ , half₁n , _) = suc half₁n

  half₂ : ℕ → ℕ
  half₂ = <-rec HalfPred half₂'
    where
    half₂' : ∀ n → <-Rec HalfPred n → HalfPred n
    half₂' zero          _   = zero
    half₂' (suc zero)    _   = zero
    half₂' (suc (suc n)) rec = suc (rec n (≤′-step ≤′-refl))
