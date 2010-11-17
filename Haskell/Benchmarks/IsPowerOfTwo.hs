module Main where

import Control.Monad
import Criterion.Main
import Data.Bits
import Data.Random
import Data.Random.Source.DevRandom

powers :: [Int]
powers = takeWhile (>0) (iterate (*2) 1)

isP2 x = case dropWhile (<x) powers of
    []      -> False
    (p:_)   -> p == x

isExact x = (x == fromIntegral (truncate x))
isP2' x = isExact (logBase 2 (fromIntegral x))

isP2'' x = go powers (x - 1)
    where 
        go [] x = False
        go (p:ps) x = case x `compare` 0 of
            LT -> False
            EQ -> True
            GT -> go ps (x - p)

isP2''' x = x .&. (x-1) == 0

main = do
    gibberish <- sampleFrom DevRandom (replicateM 10000 (uniform 0 maxBound)) :: IO [Int]
    run (nf id gibberish) 1
    defaultMain
        [ bench "isP2"    (nf (map isP2)    gibberish)
        , bench "isP2' "  (nf (map isP2')   gibberish)
        , bench "isP2''"  (nf (map isP2'')  gibberish)
        , bench "isP2'''" (nf (map isP2''') gibberish)
        ]

-- Not directly comparable, but interesting anyway:
logLength b [] = Nothing
logLength b (_:xs) = go 0 (iterate (*b) (b-1)) xs
    where
        go p     _  [] = return p
        go p (n:ns) xs = dropExact n xs >>= go (p + 1) ns

dropExact n xs = case drop (n-1) xs of
    []      -> Nothing
    (_:ys)  -> Just ys