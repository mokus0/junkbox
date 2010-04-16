{-# LANGUAGE FlexibleContexts #-}
{-
 - Tri-diagonal system solver using LU decomposition, translated
 - from C++ code given in Numerical Recipes, 3rd ed. (p. 56).
 - tridcmp is derived from the code given on p. 52 for a general LU
 - decomposition.
 -}
module NR.Ch2.S4.Tridiagonal
    ( tridag
    , tridag_u
    , tridcmp
    , triSolve
    , TriLU(..)
    ) where

import NR.Ch1.S4
import Data.List

-- test case
-- import Data.Matrix.Math
-- type T = Rational
-- a = [2,3,5,7,11]
-- b = [13,17,19,23,29]
-- c = [31,37,41,49,53]
-- m = triDiag a b c :: IMatrix T
-- r = [59,67,71,73,79]
-- x :: IVector T
-- x = tridag a b c r
-- -- chk should be all zeroes
-- chk = m `apply` x `subL` convertV r
-- lu = tridcmp a b c
-- x2 = triSolve lu r
-- chk2 = m `apply` x2 `subL` convertV r

tridag :: (Fractional t,
           Vector v1 t,
           Vector v2 t,
           Vector v3 t,
           Vector v4 t) =>
          v1 t -> v2 t -> v3 t -> v4 t -> IVector t
tridag a b c r = backsub gam u
    where (gam, u) = decomp_and_fsub a b c r

tridag_u :: (Fractional t,
             Vector v1 t,
             Vector v2 t,
             Vector v3 t,
             Vector v4 t,
             Vector UVector t) =>
            v1 t -> v2 t -> v3 t -> v4 t -> IVector t
tridag_u a b c r = backsub gam u
    where (gam, u) = decomp_and_fsub_u a b c r

-- decomposition and forward substitution, fused so that only
-- one component of beta is ever used at once
decomp_and_fsub :: (Fractional t,
                    Vector v1 t,
                    Vector v2 t,
                    Vector v3 t,
                    Vector v4 t) =>
                   v1 t -> v2 t -> v3 t -> v4 t -> (IVector t, IVector t)
decomp_and_fsub = decomp_and_fsub_g

decomp_and_fsub_u :: (Fractional t,
                      Vector v1 t,
                      Vector v2 t,
                      Vector v3 t,
                      Vector v4 t,
                      Vector UVector t) =>
                     v1 t -> v2 t -> v3 t -> v4 t -> (UVector t, UVector t)
decomp_and_fsub_u = decomp_and_fsub_g

decomp_and_fsub_g a b c r = (vectorFromList gam, vectorFromList u)
    where
        n = vecElems r
        
        (gam,u) = unzip gam_u
        -- simultaneous construction of Gamma and U vectors by accumulation.  
        -- Each step depends only on the accumulated variable "Beta" and on
        -- the previous component of U.  Beta could easily be its own vector,
        -- and I believe that would correspond to decomposition without
        -- forward substitution.
        gam_u = (gam0, u0) : snd (mapAccumL next_gam_u (b0, u0) [1..n-1])
        next_gam_u (prevBet, prevU) j = ((bet_j, u_j), (gam_j, u_j))
            where
                bet_j = indexV b j - a_j  * gam_j
                gam_j = indexV c (j-1) / prevBet
                u_j   = (indexV r j - a_j * prevU) / bet_j
                
                a_j = indexV a j
        
        b0 = indexV b 0
        u0 = indexV r 0 / b0
        gam0 = 0

fwsub :: (Fractional t,
          Vector v1 t,
          Vector v2 t,
          Vector v3 t) =>
         v1 t -> v2 t -> v3 t -> IVector t
fwsub a beta r = u
    where
        n = vecElems r 
        u = vector n $ \i -> if i == 0
            then indexV r i / indexV beta i
            else (indexV r i - indexV a i * indexV u (i-1)) / indexV beta i

backsub :: (Num t, Vector v1 t, Vector v2 t) =>
           v1 t -> v2 t -> IVector t
backsub gam u = x
    where
        n = vecElems u
        x = vector n $ \i -> if i == n-1 
            then indexV u i
            else indexV u i - indexV gam (i+1) * indexV x (i+1)


-- opposite of the normalization in Data.Matrix.Algorithms.LUDecomp.
-- L consists of A on the sub-diagonal and Beta on the diagonal.
-- U consists of 1 on the diagonal and Gamma on the super-diagonal

data TriLU v t = TriLU
    { triLUalpha    :: v t
    , triLUbeta     :: IVector t
    , triLUgamma    :: IVector t
    }

tridcmp :: (Fractional t,
           Vector v1 t,
           Vector v2 t,
           Vector v3 t) =>
          v1 t -> v2 t -> v3 t -> TriLU v1 t
tridcmp a b c = TriLU a beta gamma
    where
        n = vecElems b
        
        beta  = vector n $ \i -> if i == 0
            then indexV b i
            else indexV b i -  indexV a i * indexV gamma i
        gamma = vector n $ \i -> if i == 0
            then 0
            else indexV c (i-1) / indexV beta (i-1)

triSolve :: (Fractional t,
             Vector v1 t,
             Vector v2 t) =>
            TriLU v1 t -> v2 t -> IVector t
triSolve (TriLU alpha beta gamma) = backsub gamma . fwsub alpha beta
