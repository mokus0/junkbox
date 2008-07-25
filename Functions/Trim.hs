{-
 -      ``Trim''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Trim where

-- simple, effective; requires double traversal, i think
trimBy p = t . t
        where t = reverse . dropWhile p


-- not simple, not that fast; especially bad with long sequences
-- satisfying |p|
trimBy2 p [] = []
trimBy2 p (x:xs)
        | p x           = trimBy2 p xs
        | otherwise     = trimEnd xs
        where
                trimEnd [] = []
                trimEnd (x:xs)
                        | p x           = case trimEnd xs of
                                []      -> []
                                ys      -> x : ys
                        | otherwise     = x : trimEnd xs

trimBy3 p = trimEnd [] . dropWhile p
        where
                trimEnd ys [] = []
                trimEnd [] (x:xs)
                        | p x           = trimEnd [x] xs
                        | otherwise     = x : trimEnd xs
                trimEnd ys (x:xs)
                        | p x           = trimEnd (x:ys) xs
                        | otherwise     = reverse ys ++ x : trimEnd [] xs