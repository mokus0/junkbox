{-
 -	"Data/Permutation.hs"
 -	(c) 2008 James Cook
 -}

module Data.Permutation where

import qualified Data.List as L
import qualified Data.Map as M

newtype Perm t = Perm { toMap :: M.Map t t }
        deriving (Eq, Show)

fromList l = Perm (M.fromList (zip [1..] l))

fromCycle [] = []
fromCycle c = zip c (tail c ++ [head c])

fromCycles cs = Perm (M.fromList (concatMap fromCycle cs))

decompMap p = M.mapMaybeWithKey findCycle p
        where
                findCycle k v = if all (> k) (closure k v)
                        then Just (closure k v)
                        else Nothing
                closure k v 
                        | k == v        = []
                        | otherwise     = v : closure k (p M.! v)

decomp :: (Ord t) => Perm t -> [[t]]
decomp = filter ((> 1) . length) . map (uncurry (:)) . M.toList . decompMap . toMap

permutations 0 xs = return []
permutations n xs = do
        x <- xs
        rest <- permutations (n-1) (xs L.\\ [x])
        return (x:rest)