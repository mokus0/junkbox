{-# LANGUAGE RecordWildCards #-}

-- Deques with guaranteed O(1)-time insert/remove operations,
-- from Fig. 5 of http://www.eecs.usma.edu/webs/people/okasaki/jfp95.ps
module Data.Deque
    ( Deque
    , empty
    , size
    , insertL, insertR
    , removeL, removeR
    
    , fromListL, fromListR
    , toListL, toListR
    ) where

import Data.Bits
import Data.List (unfoldr)

-- 'balance threshold' parameter.  must be >= 2.  c = 3 is typical
c = 3

data Deque a = Deque 
    { nL, nR :: !Int
    , l, r, lHat, rHat :: ![a]
    } deriving Show

invariants Deque{..} = and
    [ nL == length l
    , nR == length r
    , nL <= c * nR + 1
    , nR <= c * nL + 1
    , length lHat <= max 0 (2 * j + 2 - k)
    , length rHat <= max 0 (2 * j + 2 - k)
    ] where j = min nL nR; k = max nL nR

empty :: Deque a
empty = Deque 0 0 [] [] [] []

size Deque{..} = nL + nR

insertL e Deque{..} = makedq (succ nL) nR (e : l) r (drop 1 lHat) (drop 1 rHat)
insertR e Deque{..} = makedq nL (succ nR) l (e : r) (drop 1 lHat) (drop 1 rHat)

removeL Deque{ l = [],  r = []}      = Nothing
removeL Deque{ l = [],  r = [e], ..} = Just (e, empty)
removeL Deque{ l = e:l, r = r,   ..} = Just (e, makedq (pred nL) nR l r (drop 2 lHat) (drop 2 rHat))

removeR Deque{l = [],  r = []     } = Nothing
removeR Deque{l = [e], r = []     } = Just (e, empty)
removeR Deque{l = l,   r = e:r, ..} = Just (e, makedq nL (pred nR) l r (drop 2 lHat) (drop 2 rHat))

makedq nL nR l r lHat rHat
    | nL > c * nR + 1 =
        let l' = take n l
            r' = rot1 n r l
         in Deque n n' l' r' l' r'
    | nR > c * nL + 1 =
        let l' = rot1 n l r
            r' = take n r
         in Deque n' n l' r' l' r'
    | otherwise = Deque{..}
    where
        n = avg nL nR
        n' = nL + nR - n

rot1 n l r
    | n >= c    = head l : rot1 (n - c) (tail l) (drop c r)
    | otherwise = rot2 l (drop n r) []

rot2 l r a
    | null l || lengthLt c r    = l ++ reverse r ++ a
    | otherwise                 = head l : rot2 (tail l) r2 (reverse r1 ++ a)
    where (r1, r2) = splitAt c r


-- floor ((x + y) / 2), but without possibility of overflow.
avg x y
    | odd x && odd y    = (x `shiftR` 1) + (y `shiftR` 1) + 1
    | otherwise         = (x `shiftR` 1) + (y `shiftR` 1)

-- if n > 0, then @lengthLt n xs == length xs < n@
lengthLt n xs = null (drop (n-1) xs)

fromListL, fromListR :: [a] -> Deque a
fromListL = foldr insertL empty
fromListR = foldr insertR empty

toListL, toListR :: Deque a -> [a]
toListL = unfoldr removeL
toListR = unfoldr removeR