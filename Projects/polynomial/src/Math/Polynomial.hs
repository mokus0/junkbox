{-# LANGUAGE ParallelListComp, ViewPatterns #-}
module Math.Polynomial
    ( Endianness(..)
    , Poly, poly, polyCoeffs
    , addPoly, multPoly
    , quotRemPoly, quotPoly, remPoly
    , evalPoly, evalPolyDeriv, evalPolyDerivs
    , polDivMon
    , prettyPoly, prettyPolyWith, prettyTerm
    , gcdPoly
    ) where

import Math.Polynomial.Type

import Data.List
import Text.PrettyPrint

prettyPoly p = prettyPolyWith (prettyTerm 'x') p

-- TODO: figure out how to handle signs better
prettyPolyWith v p = hsep $ intersperse (char '+') $ filter (not . isEmpty)
    [ v coeff exp
    | coeff <- polyCoeffs BE p
    | exp <- [0..]
    ]

prettyTerm v 0 e = empty
prettyTerm v c 0 = text (show c)
prettyTerm v 1 1 = char v
prettyTerm v c 1 = text (show c) <> char v
prettyTerm v 1 e = char v <> text "^" <> int e
prettyTerm v c e = text (show c) <> char v <> text "^" <> int e

addPoly  (polyCoeffs LE ->  a) (polyCoeffs LE ->  b) = poly LE (zipSum a b) 
multPoly (polyCoeffs LE -> xs) (polyCoeffs LE -> ys) = poly LE $ foldl zipSum []
    [ map (x *) (shift ++ ys)
    | x <- xs
    | shift <- inits (repeat 0)
    ]

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
                u' = tail (zipWith (-) u (map (q0 *) (v ++ repeat 0)))

quotPoly u v = fst (quotRemPoly u v)
remPoly  u v = snd (quotRemPoly u v)

zipSum xs [] = xs
zipSum [] ys = ys
zipSum (x:xs) (y:ys) = (x+y) : zipSum xs ys

evalPoly (polyCoeffs LE -> cs) x = foldr mul 0 cs
    where
        mul c acc = c + acc * x

evalPolyDeriv (polyCoeffs LE -> cs) x = foldr mul (0,0) cs
    where
        mul c (p, dp) = (p * x + c, dp * x + p)

evalPolyDerivs (polyCoeffs LE -> cs) x = trunc . zipWith (*) factorials $ foldr mul (repeat 0) (zip cs [0..])
    where
        trunc list = zipWith const list cs
        factorials = scanl (*) 1 (iterate (+1) 1)
        mul (c, i) pds@(p:pd) = (p * x + c) : map (x *) pd `zipSum` pds

-- polDivMon : divide a polynomial P by a monomial (x - a)
polDivMon (polyCoeffs LE -> cs) a = (poly LE q, r)
    where 
        (r,q) = mapAccumR (\rem swap -> (swap + rem * a, rem)) 0 cs

gcdPoly a 0  =  monic a
gcdPoly a b  =  gcdPoly b (a `remPoly` b)

monic p = case polyCoeffs BE p of
    []      -> poly BE []
    (x:xs)  -> poly BE (1:map (/x) xs)
