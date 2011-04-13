------------------------------------------------------------------------
-- Properties satisfied by strict partial orders
------------------------------------------------------------------------

{-# OPTIONS --universe-polymorphism #-}

open import Relation.Binary

module Relation.Binary.Props.StrictTotalOrder
       {s₁ s₂ s₃} (STO : StrictTotalOrder s₁ s₂ s₃)
       where

open Relation.Binary.StrictTotalOrder STO
import Relation.Binary.StrictToNonStrict as Conv
open Conv _≈_ _<_
import Relation.Binary.Props.StrictPartialOrder as SPO
open import Relation.Binary.Consequences

------------------------------------------------------------------------
-- Strict total orders can be converted to decidable total orders

decTotalOrder : DecTotalOrder _ _ _
decTotalOrder = record
  { isDecTotalOrder = record
    { isTotalOrder = record
      { isPartialOrder = SPO.isPartialOrder strictPartialOrder
      ; total          = total compare
      }
    ; _≟_  = _≟_
    ; _≤?_ = decidable' compare
    }
  }

open DecTotalOrder decTotalOrder public
