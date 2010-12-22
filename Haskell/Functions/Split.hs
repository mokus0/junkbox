module Split where

import Data.List
import Safe

splitOn1, splitOn2 :: (a -> Bool) -> [a] -> [[a]]

-- This one incorrectly handles the case where (\xs -> p (head xs)) - The
-- head of the input can never be recognized as a delimiter.
splitOn1 p = mapTail tail . groupBy (const (not.p))
    where
        mapTail f []     = []
        mapTail f (x:xs) = x : map f xs

-- This correctly handles the start of the list and also is clearer
--  (if you understand the concept of an unfold).
-- However, when the list ends with a delimiter, it doesn't return a final
-- empty component.
splitOn2 p = unfoldr splitOnce
    where
        splitOnce [] = Nothing
        splitOnce xs = Just (pre, drop 1 post)
            where ~(pre, post) = break p xs

-- This one is, as far as I can tell, entirely correct (but ugly)
splitOn3 p [] = []
splitOn3 p [x]
    | p x       = [[],[]]
splitOn3 p (x:xs)
    | p x       = [] : splitOn3 p xs
    | otherwise = splitOn1 p (x:xs)

splitOn4 p = loop
    where
        loop xs = case break p xs of
            (pre, [])     -> pre : []
            (pre, _:post) -> pre : loop post

-- Here's a nice concise and correct implementation, finally.
-- It makes use of an 'withTailOf' function which is pretty simple but still
-- requires a little explanation.
splitOn5 p xs = pre : withTailOf post (splitOn5 p)
    where ~(pre,post) = break p xs

withTailOf []     f = []
withTailOf (_:xs) f = f xs

-- This one is arguably a bit simpler since it uses existing library functions.
splitOn6 p xs = pre : maybe [] (splitOn6 p) (tailMay post)
    where ~(pre,post) = break p xs

test splitOn delim xs
    =  max 0 (length (splitOn delim xs) - 1)
    == count delim xs

count p = length . filter p
