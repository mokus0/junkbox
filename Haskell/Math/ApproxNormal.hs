-- some quick and ugly probability calculations to test variations on
-- an algorithm for approximately sampling an normal distribution on a
-- rather small microcontroller.  The goal is to get "decent" results
-- without using any floating-point math or any tables, since the target
-- only has 512 words of program flash and 64 bytes of RAM.
{-# LANGUAGE TupleSections #-}
module Math.ApproxNormal where

import Control.Arrow
import Control.Applicative
import Data.Bits
import Data.Int
import qualified Data.Vector as V
import Data.VectorSpace
import Data.Word
import Monads.VectorSpace

import qualified Statistics.Distribution as S
import qualified Statistics.Distribution.Normal as S
import qualified Statistics.Sample as SS

bernoulli p = v (V.fromList [(1, p), (0, 1-p)])

-- mean  = n*p
-- stdev = sqrt(n*p*(1-p))
binomial n p
    | n <= 0    = return 0
    | n == 1    = bernoulli p
    | even n    = reduce $ do
        let n2 = n `div` 2
        a <- binomial n2 p
        b <- binomial n2 p
        return (a + b)
    | otherwise = reduce $ do
        a <- bernoulli p
        b <- binomial (n - 1) p
        return (a + b)

uniform n = v (V.map (,p) (V.enumFromTo 0 (n-1)))
    where p = recip (fromIntegral n)

normal a b c d = reduce $ do
    center  <- (a +) . (b *) <$> binomial c 0.5
    fuzz    <- reduce (subtract <$> uniform d <*> uniform d)
    return (center + fuzz)

normal8 :: V Double Int8
normal8 = normal (-128) 16 16 16

-- dist8 :: S.ContDistr d => d -> V Double Int8
dist8 d = bayes (v (V.map (\n -> (n, S.density d (fromIntegral n))) (V.enumFromTo (-128) 127)))

fit x = uncurry S.normalDistr ((SS.meanWeighted &&& sqrt . SS.varianceWeighted) (unV (fromIntegral <$> x)))

-- compare 2 distributions, reporting maximum absolute difference of
-- bin probabilities, mean absolute and RMS differences.
distDiff x y = (V.maximum (V.cons 0 d), SS.mean d, sqrt (SS.mean (V.map (^2) d)))
    where d = V.map (abs . snd) (unV (reduce (x ^-^ y)))

-- test the "normality" of a distribution
test x = (distDiff x (dist8 d), d)
    where d = fit x

-- tried some variations with retry... this simpler version actually
-- has better statistics for the values actually used by the application.
shiftedNormal :: Word8 -> Word8 -> V Double Word8
shiftedNormal m s = reduce $ do
    r <- normal8
    let x = fromIntegral m + ((fromIntegral s * fromIntegral r) `shiftR` 5) :: Int16
    if x < 0
        then return 0
        else if x > 255
            then return 255
            else return $ fromIntegral x
