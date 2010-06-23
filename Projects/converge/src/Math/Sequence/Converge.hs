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

-- |@convergeTo absEps relEps xs@ takes items from @xs@ until two successive
-- items @x@ and @y@ are within either @absEps@ or @relEps * max (abs x) (abs
-- y)@ of each other, in which case the second of the pair is returned, or 
-- until an item is found that does not equal itself (which would typically 
-- be a NaN), in which case the preceding item is returned.  If the list ends
-- before a match is found, the last element of the list is returned.
--
-- For example, approximating the golden mean by applying Newton's method to 
-- find a root of @x^2 - x - 1@:
-- 
-- > phi :: Rational
-- > phi = convergeTo 1e-100 0 (iterate (\x -> (x*x + 1) / (2*x-1)) 1)
convergeTo :: (Fractional a, Ord a) => a -> a -> [a] -> a
convergeTo absEps relEps = fromMaybe empty . convergeBy eq Just
    where
        empty = error "convergeTo: empty list"
        eq (a:b:_)
            | b /= b                            = Just a
            | absDiff <= abs absEps             = Just b
            | absDiff <= abs relEps * relScale  = Just b
            where
                absDiff = abs (a-b)
                relScale = max (abs a) (abs b)
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
        f' xs = case f xs of
            Nothing -> end' xs
            other   -> other
        end' [x] = end x
        end'  _  = Nothing