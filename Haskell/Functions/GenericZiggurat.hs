{-
 -      ``GenericZiggurat''
 -      (c) 2009 James Cook
 -}
{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances, FlexibleContexts,
        RecordWildCards
  #-}

module GenericZiggurat where

import Data.Random.Internal.Words
import Data.Bits

import Data.Array
import Numeric.Search.Bounded
import Debug.Trace

import Data.Random.Distribution.Normal (erf)
import Data.Random.Distribution.Uniform
import Data.Random.Distribution
import Data.Random.RVar

data Ziggurat t = Ziggurat
    { zTable            :: Array Int t
    , zPrecompRatios    :: Array Int t
    , zPrecomp_fxi      :: Array Int t
    , zGetIU            :: RVar (Int, t)
    , zTailDist         :: Bool -> RVar t
    , zUniform          :: t -> t -> RVar t
    , zFunc             :: t -> t
    , zMirror           :: Bool
    }

{-# INLINE runZiggurat #-}
runZiggurat Ziggurat{..} = go
    where
        go = do
            (i,u) <- zGetIU
            let x = u * zTable ! i
            
            if abs u < zPrecompRatios ! i
                then return $! x
                else if i == 0
                    then zTailDist (x < 0)
                    else do
                        v <- zUniform (zPrecomp_fxi ! (i+1)) (zPrecomp_fxi ! i)
                        if v < zFunc x
                            then return $! x
                            else go

mkZiggurat m f fInv c r v tailDist = z
    where z@Ziggurat{..} = Ziggurat
            { zTable            = zigguratTable f fInv c r v
            , zPrecompRatios    = precomputeRatios zTable
            , zPrecomp_fxi      = fmap f zTable -- sampleArray (0,c)   $ \i -> f (zTable!i)
            , zGetIU            = do
                i <- uniform 0 (c-1)
                u <- stdUniform
                return (i,if m then u+u-1 else u)
            , zUniform = uniform
            , zFunc = f
            , zTailDist = tailDist
            , zMirror = m
            }

zigguratTable :: (Integral i, Ix i, Fractional a) =>
                 (a -> a) -> (a -> a) -> i -> a -> a -> Array i a
zigguratTable f fInv c r v = xs
    where
        xs = sampleArray (0,c) x
        
        x 0 = v / f (xs ! 1)
        x 1 = r
        x i | i == c = 0
        x (i+1) = fInv (f (xs!i) + (v / (xs!i)))

precomputeRatios zTable = sampleArray (0,c-1) $ \i -> zTable!(i+1) / zTable!i
    where
        (0,c) = bounds zTable

sampleArray (a,b) f = listArray (a,b) [f i | i <- [a..b]]

instance (Num t, Ord t) => Distribution Ziggurat t where
    rvar = runZiggurat


findBin0 cInt f fInt totalVol = (r,totalVol / c)
    where
        c = fromIntegral cInt
        -- totalVol = fInt (findMax (not . isInfinite))
        v r = r * f r + totalVol - fInt r
        r = findMin (\r -> totalVol - (r * f r + totalVol - fInt r) * c  >= 0)

f x
    | x <= 0    = 1
    | otherwise = exp (-0.5 * x*x)

fInt x 
    | x <= 0    = 0
    | otherwise = fVol * erf (x * sqrt 0.5)

fVol :: Floating a => a
fVol = sqrt (0.5 * pi)
-- fVol = 1.2688080685135577

invertCdf f 0 = findMax (\x -> f x <= 0 && not (isInfinite x))
invertCdf f y
    | y < 0 
    = error "invertCdf: y < 0"
    | y > 1 
    = error "invertCdf: y > 1"
    | otherwise
    = fixZero (findMin (\x -> f x >= y && not (isInfinite x)))

-- eliminate negative zero, which, in many domains, is technically
-- a feasible answer
fixZero 0 = 0
fixZero z = z

invertAntitonePdf f maxY y
    | y < 0
    = error "invertAntitonePdf: y < 0"
    | y > maxY
    = error "invertAntitonePdf: y > maxY"
    | y == maxY
    = findMax (\x -> f x >= maxY && not (isInfinite x))
    | otherwise
    = fixZero (findMin (\x -> f x <= y && not (isInfinite x)))

maxNotInfinite :: RealFloat a => a
maxNotInfinite = bigNum
    where
        bigNum = fromRat bigRat
        bigRat = findMaxBy (epsCmp 1) (not.isInfinite.fromRat)
        fromRat x = fromRational x `asTypeOf` bigNum

maxFloat :: RealFloat a => a
maxFloat = x
    where
        x = fromInteger bigInt
        bigInt = rad ^ maxExp - rad ^ (maxExp - digits)
        
        rad = floatRadix x
        digits = floatDigits x
        (_,maxExp) = floatRange x

epsCmp e x y = trace ("epsCmp " ++ show (x,y)) $ compare 0 (epsSign (y-x))
    where
        epsSign z
            | abs z < e = 0
            | otherwise = z

findMaxBy compare p = negate (findMinBy compare (p.negate))
findMax p = negate (findMin (p.negate))

findMin p = findMinBy compare p
findMinBy compare p
    | p 0   = descend (-1) 0
    | otherwise
    = fixZero (ascend 0 1)
    where
        -- preconditions:
        -- not (p l)
        -- 0 <= l < x
        ascend l x 
--             = trace ("a " ++ show (l,x)) $ a l x
--         a l x
            | p x       = bisect l x
            | otherwise = ascend x $! x+x
        
        -- preconditions:
        -- p h
        -- x < h <= 0
        descend x h 
--             = trace ("d " ++ show (x,h)) $ d x h
--         d x h
            | p x       = (descend $! (x+x)) x
            | otherwise = bisect x h
        
        -- preconditions:
        -- not (p l)
        -- p h
        -- l <= h
        bisect l h 
--             = trace ("b " ++ show (l,h)) $ b l h
--         b l h
            | l >= h    = h
            | l >= mid || mid >= h
            = if p mid then mid else h
            | p mid     = bisect l mid
            | otherwise = bisect mid h
            where 
                a >= b = not (a `compare` b == LT)
                mid = (l+h)*0.5

-- |Convert an existing Ziggurat to a new type.
-- useful for example in computing tables for a
-- type that is Fractional but not Floating, such as
-- LogFloat
zMap :: (Fractional b, Distribution Uniform b) =>
        (a -> b) -> (b -> b) -> Ziggurat a -> Ziggurat b
zMap oldToNew newF z@Ziggurat{..} = z
    { zTable            = newZTable
    , zPrecompRatios    = precomputeRatios newZTable
    , zPrecomp_fxi      = fmap newF newZTable
    , zGetIU            = do
        (i,u) <- zGetIU
        return (i, oldToNew u)
    , zTailDist         = fmap oldToNew . zTailDist
    , zUniform          = uniform
    , zFunc             = newF
    }
    
    where 
        newZTable = fmap oldToNew zTable

