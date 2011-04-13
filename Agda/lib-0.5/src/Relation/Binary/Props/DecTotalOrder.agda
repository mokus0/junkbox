------------------------------------------------------------------------
-- Properties satisfied by decidable total orders
------------------------------------------------------------------------

{-# OPTIONS --universe-polymorphism #-}

open import Relation.Binary

module Relation.Binary.Props.DecTotalOrder
         {d₁ d₂ d₃} (DT : DecTotalOrder d₁ d₂ d₃) where

open Relation.Binary.DecTotalOrder DT hiding (trans)
import Relation.Binary.NonStrictToStrict as Conv
open Conv _≈_ _≤_

strictTotalOrder : StrictTotalOrder _ _ _
strictTotalOrder = record
  { isStrictTotalOrder = record
      { isEquivalence = isEquivalence
      ; trans         = trans isPartialOrder
      ; compare       = trichotomous Eq.sym _≟_ antisym total
      ; <-resp-≈      = <-resp-≈ isEquivalence ≤-resp-≈
      }
  }

open StrictTotalOrder strictTotalOrder public
