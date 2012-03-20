module modtest where

module A (T : Set) (x y : T) where
    z : T
    z = x

open A public
-- type of Z is (T : Set) (x y : T) → T
-- (quantification is over all module params, not just the ones used as in Coq)
