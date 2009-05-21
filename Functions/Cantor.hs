{-
 -      ``Cantor''
 -      RVar to sample from the Cantor distribution
 -}
{-# LANGUAGE
        MultiParamTypeClasses,
        ParallelListComp
  #-}


module Cantor where

import Data.Bits
import Data.Word
import ConvergingSum
import Control.Monad

import Data.Random

cantor :: RVar Double
cantor = wordToCantorM 1 getRandomWord

wordToCantorM :: Monad m => Double -> m Word64 -> m Double
wordToCantorM b w = do
    (x, c) <- liftM (wordToCantorC b) w
    if c
        then do
            x2 <- wordToCantorM (b * recip_3_64) w
            return (x+x2)
        else return x

recip_3_64 :: Double
recip_3_64 = recip (3 ** 64)

wordToCantorC :: Double -> Word64 -> (Double, Bool)
wordToCantorC b w = (x, c)
    where
        x = convergingSum 0 (cantorDigits b w)
        dx = b * recip_3_65
        
        c = x + dx /= x

recip_3_65 :: Double
recip_3_65 = recip (3 ** 65)

cantorDigits :: Double -> Word64 -> [Double]
cantorDigits b w = filter (/= 0)
    [ if testBit w i
        then q
        else 0
    | i <- [0..63]
    | q <- drop 1 (iterate (* recip 3) (2 * b))
    ]

cantorFunc :: Double -> Double
cantorFunc x = convergingSum 0
    [ b2
    | (b3, b2) <- zip 
        (cantorBits x)
        (iterate (*0.5) 0.5)
    , b3
    ]

toB3 :: Double -> [Int]
toB3 x = go x (recip 3)
    where
        go x q
            | x <= 0    = []
            | x >= 2*q  = 2 : go (x - 2*q) (q * recip_3)
            | x == q    = repeat 2
            | x >  q    = 1 : go (x - q)   (q * recip_3)
            | otherwise = 0 : go  x        (q * recip_3)

cantorBits :: Double -> [Bool]
cantorBits = go . toB3
    where
        go [] = []
        go (1:xs) = [True]
        go (2:xs) = True:go xs
        go (0:xs) = False:go xs
            

recip_3 :: Double
recip_3 = recip 3

data Cantor a = Cantor

instance Distribution Cantor Double where
    rvar Cantor = cantor
--     cdf  Cantor = cantorFunc