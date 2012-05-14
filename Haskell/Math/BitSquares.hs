module Math.BitSquares where

import Data.List
import qualified Data.Map as M
import Data.Maybe

squares a b = max 0 (squaresLTE b - squaresLTE (a-1))

squaresLTE x = sum [ k | (n,k) <- popCounts x, isSquare n ]

popCounts = M.toList . popCountsMap

popCountsMap n 
    | n < 0     = M.empty
    | otherwise = M.unionWith (+) (popCountsLg2 l) (M.mapKeysMonotonic (1+) (popCountsMap r))
    where (l,r) = lg2Rem n

popCountsLg2 n = M.fromList [(k, bico n k) | k <- [0..n]]

isSquare n = n `elem` takeWhile (<= n) (map (^2) [0..])

bico n 0 = 1
bico 0 k = 0
bico n k = bico (n-1) (k-1) * n `div` k

lg2Rem 0 = (0, -1)
lg2Rem n = (l, n-2^l)
    where l = fromJust (findIndex (>n) (iterate (2*) 1)) - 1