-- |A very simple FIFO which appears on the surface to have O(1) 
-- worst-case time for all queue operations, but actually does not.
--
-- Essentially, all the work 'reverse' would have done in the naive
-- two-list implementation is just shuffled \"under the rug\" to be performed
-- by the machinery implementing evaluation (e.g., the STG machine).
-- 
-- In the language of the STG execution model, a chain of 'snoc' closures
-- is built up by 'enqueue'.  That chain is almost identical to the 
-- one representing the 'inbox' list in a naive implementation, the only 
-- difference being that the info-pointer for the (:) constructor is replaced
-- by one for the 'snoc' function, and the arguments are swapped.  When
-- 'toList inbox' is eventually entered by the pattern matching in
-- 'dequeueConverted', the chain of 'snoc'-cells is traversed all the way 
-- to the end in a search for the first constructor (i.e., the innermost
-- 'snoc'), which returns the list's head and a chain of (:) closures 
-- containing the now-reversed list.
-- 
-- Operationally, this is _exactly_ how 'reverse' would have been evaluated.

module Experiments.FIFO
    ( FIFO
    , empty
    , enqueue
    , dequeue
    ) where

import Data.DList (DList, snoc, toList)
import qualified Data.DList as DList

data FIFO a = FIFO
    { inbox :: DList a
    , outbox :: [a]
    }

empty :: FIFO a
empty = FIFO (DList.empty) []

enqueue :: a -> FIFO a -> FIFO a
enqueue x (FIFO inbox outbox) = FIFO (snoc inbox x) outbox

dequeue :: FIFO a -> Maybe (a, FIFO a)
dequeue = dequeueConverted . convert
    where
        convert (FIFO inbox []) = FIFO (DList.empty) (toList inbox)
        convert other = other
        
        dequeueConverted (FIFO inbox []          ) = Nothing
        dequeueConverted (FIFO inbox (x : outbox)) = Just (x , FIFO inbox outbox)
