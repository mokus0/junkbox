module Math.Sequence.Converge where

import Data.List
import Data.Maybe
import Data.Monoid

-- |Take items from the list until two successive items are equal and 
-- return the second of them (or an item is not equal to itself, to handle
-- NaN without a 'RealFloat' context.  In this case, the first item of the 
-- pair is returned) .  If the list ends before a match is found, 
-- returns the last element of the list.
converge :: Eq a => [a] -> a
converge = fromMaybe empty . convergeBy eq Just
    where
        empty = error "converge: empty list"
        
        eq (a:b:_)
            | a == b        = Just b
            | b /= b        = Just a
        eq _ = Nothing

-- |@convergeBy f end xs@ looks through @xs@ for the first segment for which
-- @f@ returns a value, and returns that value.  Typically @f@ would be 
-- something like:
-- 
-- > f (a:b:_)
-- >    | abs(a-b) <= eps
-- >    = Just (0.5 * (a + b))
-- > f _ = Nothing
-- 
-- If no such segment is found, applies @end@ to the last item in the list
-- and returns the result.  If the list was empty, returns 'Nothing'.
-- 
convergeBy :: ([a] -> Maybe b) -> (a -> Maybe b) -> [a] -> Maybe b
convergeBy f end = listToMaybe . catMaybes . map f' . tails
    where 
        f' xs = maybe (if null (drop 1 xs) then end (head xs) else Nothing) Just (f xs)
