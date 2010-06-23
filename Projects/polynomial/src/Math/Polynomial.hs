{-# LANGUAGE ParallelListComp, ViewPatterns #-}
module Math.Polynomial
    ( Endianness(..)
    , Poly, poly, polyCoeffs, polyIsZero
    , addPoly, multPoly
    , quotRemPoly, quotPoly, remPoly
    , evalPoly, evalPolyDeriv, evalPolyDerivs
    , contractPoly
    , gcdPoly
    ) where

import Math.Polynomial.Type
import Math.Polynomial.Pretty ({- instance -})

import Data.List

addPoly :: Num a => Poly a -> Poly a -> Poly a
addPoly  (polyCoeffs LE ->  a) (polyCoeffs LE ->  b) = poly LE (zipSum a b) 

multPoly :: Num a => Poly a -> Poly a -> Poly a
multPoly (polyCoeffs LE -> xs) (polyCoeffs LE -> ys) = poly LE $ foldl zipSum []
    [ map (x *) (shift ++ ys)
    | x <- xs
    | shift <- inits (repeat 0)
    ]

quotRemPoly :: Fractional a => Poly a -> Poly a -> (Poly a, Poly a)
quotRemPoly (polyCoeffs BE -> u) (polyCoeffs BE -> v)
    = go [] u (length u - length v)
    where
        v0  | null v    = 0
            | otherwise = head v
        go q u n
            | null u || n < 0   = (poly LE q, poly BE u)
            | otherwise         = go (q0:q) u' (n-1)
            where
                q0 = head u / v0
                u' = tail (zipSum u (map (negate q0 *) v))

quotPoly :: Fractional a => Poly a -> Poly a -> Poly a
quotPoly u v = fst (quotRemPoly u v)
remPoly :: Fractional a => Poly a -> Poly a -> Poly a
remPoly  u v = snd (quotRemPoly u v)

-- like @zipWith (+)@ except that when the end of a list is
-- reached, it is padded with 0's to match the length of the other list.
zipSum xs [] = xs
zipSum [] ys = ys
zipSum (x:xs) (y:ys) = (x+y) : zipSum xs ys

evalPoly :: Num a => Poly a -> a -> a
evalPoly (polyCoeffs LE -> cs) x = foldr mul 0 cs
    where
        mul c acc = c + acc * x

evalPolyDeriv :: Num a => Poly a -> a -> (a,a)
evalPolyDeriv (polyCoeffs LE -> cs) x = foldr mul (0,0) cs
    where
        mul c (p, dp) = (p * x + c, dp * x + p)

evalPolyDerivs :: Num a => Poly a -> a -> [a]
evalPolyDerivs (polyCoeffs LE -> cs) x = trunc . zipWith (*) factorials $ foldr mul (repeat 0) (zip cs [0..])
    where
        trunc list = zipWith const list cs
        factorials = scanl (*) 1 (iterate (+1) 1)
        mul (c, i) pds@(p:pd) = (p * x + c) : map (x *) pd `zipSum` pds

-- |\"Contract\" a polynomial by attempting to divide out a root.
--
-- @contractPoly p a@ returns @(q,r)@ such that @q*(x-a) + r == p@
contractPoly :: Num a => Poly a -> a -> (Poly a, a)
contractPoly (polyCoeffs LE -> cs) a = (poly LE q, r)
    where 
        (r,q) = mapAccumR (\rem swap -> (swap + rem * a, rem)) 0 cs

gcdPoly :: Fractional a => Poly a -> Poly a -> Poly a
gcdPoly a (polyIsZero -> True)      =  monic a
gcdPoly a b                         =  gcdPoly b (a `remPoly` b)

monic p = case polyCoeffs BE p of
    []      -> poly BE []
    (x:xs)  -> poly BE (1:map (/x) xs)
