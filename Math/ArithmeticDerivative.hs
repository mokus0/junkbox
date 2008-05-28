{-
 -      ``ArithmeticDerivative''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module ArithmeticDerivative where

import Primes3
import Control.Monad

isPrime n = elt primes
        where
                elt (p:ps) = case p `compare` n of
                        LT      -> elt ps
                        EQ      -> True
                        GT      -> False

ad x
        | x < 0         = negate (ad (negate x))
        | x <= 1        = 0
        | isPrime x     = 1
        | otherwise     = a * ad b + ad a * b
        where
                (a, b) = head $ do -- List monad
                        a <- primes
                        (b,0) <- return (x `divMod` a)
                        return (a,b)