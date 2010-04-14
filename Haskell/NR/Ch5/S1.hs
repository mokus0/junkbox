{-# LANGUAGE ParallelListComp #-}
module NR.Ch5.S1
    ( Poly, polyBE, polyLE, polyCoeffsBE, polyCoeffsLE
    , addPoly, negatePoly, multPoly
    , quotRemPoly, quotPoly, remPoly
    , evalPoly, evalPolyDeriv, evalPolyDerivs
    , polDivMon
    ) where

import Data.List
import Data.Monoid
import Functions.Trim (dropEnd)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic as GV
import Control.Monad.ST
import Text.PrettyPrint

-- |Make a Poly from a Little-Endian list of coefficients (head is const term)
polyLE cs = PolyLE (dropEnd (0==) cs)
polyCoeffsLE (PolyLE cs) = cs

-- |Make a Poly from a Big-Endian list of coefficients (head is x^n term)
polyBE cs = polyLE (reverse cs)
polyCoeffsBE p = reverse (polyCoeffsLE p)

-- |Polynomials represented as lists of coefficients.  The 'head' of the list
-- is the constant term, the 'last' is the x^n term.

-- INVARIANT: no trailing 0's
newtype Poly a = PolyLE [a] deriving (Eq, Ord)
instance Show a => Show (Poly a) where
    showsPrec p (PolyLE cs) = showParen (p > 10) (showString "polyLE " . showsPrec 11 cs)

prettyPoly p = prettyPolyWith (prettyTerm 'x') p

-- TODO: figure out how to handle signs better
prettyPolyWith v p = hsep $ intersperse (char '+') $ reverse $ filter (not . isEmpty)
    [ v coeff exp
    | coeff <- polyCoeffsLE p
    | exp <- [0..]
    ]

prettyTerm v 0 e = empty
prettyTerm v c 0 = text (show c)
prettyTerm v 1 1 = char v
prettyTerm v c 1 = text (show c) <> char v
prettyTerm v 1 e = char v <> text "^" <> int e
prettyTerm v c e = text (show c) <> char v <> text "^" <> int e

addPoly (PolyLE a) (PolyLE b) = polyLE (zipSum a b) 
negatePoly (PolyLE a) = PolyLE (map negate a)
multPoly (PolyLE xs) (PolyLE ys) = polyLE $ foldl zipSum []
    [ map (x *) (shift ++ ys)
    | x <- xs
    | shift <- inits (repeat 0)
    ]
quotRemPoly (PolyLE u) (PolyLE v) = case polDivST (/) (V.fromList u) (V.fromList v) of
    (q,r) -> (polyLE (V.toList q), polyLE (V.toList r))

quotPoly u v = fst (quotRemPoly u v)
remPoly  u v = snd (quotRemPoly u v)

zipSum xs [] = xs
zipSum [] ys = ys
zipSum (x:xs) (y:ys) = (x+y) : zipSum xs ys

evalPoly (PolyLE cs) x = foldr mul 0 cs
    where
        mul c acc = c + acc * x

evalPolyDeriv (PolyLE cs) x = foldr mul (0,0) cs
    where
        mul c (p, dp) = (p * x + c, dp * x + p)

evalPolyDerivs (PolyLE cs) x = trunc . zipWith (*) factorials $ foldr mul (repeat 0) (zip cs [0..])
    where
        trunc list = zipWith const list cs
        factorials = scanl (*) 1 (iterate (+1) 1)
        mul (c, i) pds@(p:pd) = (p * x + c) : map (x *) pd `zipSum` pds

-- polDivMon : divide a polynomial P by a monomial (x - a)
polDivMon (PolyLE cs) a = (polyLE q, r)
    where 
        (r,q) = mapAccumR (\rem swap -> (swap + rem * a, rem)) 0 cs

polDivST (/) u v = runST $ do
    let nv = V.length v - 1
        n  = V.length u - 1
        
    -- initialize output arrays
    q <- MV.newWith (V.length u) 0
    r <- MV.newWith (V.length u) 0
    sequence_ [ MV.write r i (u V.! i) | i <- [0..n]]
    
    -- perform the division
    sequence_
        [ do
            -- q[k] = r[nv+k] / v[nv]
            r_nvpk <- MV.read r (nv+k)
            let q_k = r_nvpk / (v V.! nv)
            MV.write q k q_k
            sequence_
                [ do
                    -- r[j] -= q[k] * v[j-k]
                    r_j <- MV.read r j
                    MV.write r j $! (r_j - q_k * (v V.! (j-k)))
                | j <- [nv + k - 1, nv + k - 2 .. k]
                ]
        | k <- [n-nv , n-nv-1 .. 0]
        ]
    -- zero out excess in r
    sequence_
        [ MV.write r j 0
        | j <- [nv .. n]
        ]
    
    q <- GV.unsafeFreeze q
    r <- GV.unsafeFreeze r
    return ((q,r) `asTypeOf` (u,u))
