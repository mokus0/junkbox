-- |Queues with pre-evaluation, from
-- http://www.eecs.usma.edu/webs/people/okasaki/jfp95.ps
module Data.FIFO
    ( FIFO
    , empty
    , size
    , insert
    , remove
    
    , null
    , fromList
    , toList
    ) where

import Data.List (unfoldr)
import Data.Maybe (isNothing)
import Prelude hiding (null)

-- Almost-direct translation of Figure 4
-- ('remove' has been made total)
--
-- all operations except 'size' take O(1) time to evaluate to WHNF.

data FIFO a = FIFO ![a] ![a] ![a]

empty = FIFO [] [] []

size (FIFO l r _) = length l + length r

insert e (FIFO l r lHat) = makeQ l (e:r) lHat

remove (FIFO  []   r lHat) = Nothing
remove (FIFO (e:l) r lHat) = Just (e, makeQ l r lHat)

-- precondition: length lHat = length l - length r + 1
-- forces one cell of lHat at every queue operation, guaranteeing that
-- pattern matches on 'l' always take O(1) time.  ('lHat' is always a
-- suffix of 'l')
makeQ l r []        = let l' = rot l r [] in FIFO l' [] l'
makeQ l r (_:lHat)  = FIFO l r lHat

-- precondition: length r = length l + 1
-- this is a fusion of (l ++ reverse r) which evaluates one step of reverse
-- for each step of (++), guaranteeing that by the time any element of 'r' 
-- is demanded, 'r' has been fully reversed ('a' is an accumulator for the
-- reversed part of 'r').
rot []     [r]    a = r : a
rot (l:ls) (r:rs) a = l : rot ls rs (r:a)

-- End figure 4 --

null :: FIFO a -> Bool
null = isNothing . remove

fromList :: [a] -> FIFO a
fromList = foldr insert empty

toList :: FIFO a -> [a]
toList = unfoldr remove

instance Show a => Show (FIFO a) where
    showsPrec p fifo
        | null fifo = showString "empty"
        | otherwise = showParen (p > 10)
            ( showString "fromList "
            . showsPrec 11 (toList fifo)
            )