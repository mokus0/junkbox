{-
 -      ``Set.hs''
 -      (c) 2008 James Cook
 -}

module Set where

import Data.List

isAttrSet :: Ord a => [a] -> Bool
isAttrSet [] = True
isAttrSet xs = length grouped == 1 || maximum groupLengths == 1
        where
                grouped = group (sort xs)
                groupLengths = map length grouped

isSet :: Ord a => [[a]] -> Bool
isSet = all isAttrSet . transpose