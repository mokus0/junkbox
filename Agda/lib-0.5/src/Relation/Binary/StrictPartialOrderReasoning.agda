------------------------------------------------------------------------
-- Convenient syntax for "equational reasoning" using a strict partial
-- order
------------------------------------------------------------------------

{-# OPTIONS --universe-polymorphism #-}

open import Relation.Binary

module Relation.Binary.StrictPartialOrderReasoning
         {p₁ p₂ p₃} (S : StrictPartialOrder p₁ p₂ p₃) where

import Relation.Binary.PreorderReasoning as PreR
import Relation.Binary.Props.StrictPartialOrder as SPO
open PreR (SPO.preorder S) public renaming (_∼⟨_⟩_ to _<⟨_⟩_)
