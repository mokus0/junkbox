module PatternMerge where

import Control.Monad
import Data.List
import Data.Maybe
import Language.Haskell.TH

-- TODO: check for multiple bindings of same vars?
mergePat :: (Pat, a) -> (Pat, b) -> [(Pat, Maybe a, Maybe b)]
mergePat (WildP, a) (p, b)  = [(p, Just a, Just b), (WildP, Just a, Nothing)]
mergePat (p, a) (WildP, b)  = [(p, Just a, Just b), (WildP, Nothing, Just b)]
mergePat (VarP n, a) (p, b) = [(AsP n p, Just a, Just b), (VarP n, Just a, Nothing)]
mergePat (p, a) (VarP n, b) = [(AsP n p, Just a, Just b), (VarP n, Nothing, Just b)]
mergePat (ConP n1 p1, a) (ConP n2 p2, b)
    | n1 == n2 && length p1 == length p2
    = [ (ConP n1 ps, if mA then Just a else Nothing, if mB then Just b else Nothing) 
      | (ps, mA, mB) <- map collate (zipWithM (\x y -> mergePat (x,a) (y,b)) p1 p2)
      , mA || mB
      ]
mergePat (LitP l1, a) (LitP l2, b)
    | l1 == l2  = [(LitP l1, Just a, Just b)]
mergePat (TupP p1, a) (TupP p2, b)
    | length p1 == length p2
    = [ (TupP ps, if mA then Just a else Nothing, if mB then Just b else Nothing) 
      | (ps, mA, mB) <- map collate (zipWithM (\x y -> mergePat (x,a) (y,b)) p1 p2)
      , mA || mB
      ]
-- TODO: implement other cases!
mergePat (p1, a) (p2, b) = [(p1, Just a, Nothing), (p2, Nothing, Just b)]

collate :: [(Pat, Maybe a, Maybe b)] -> ([Pat], Bool, Bool)
collate it = (pats, all isJust as, all isJust bs)
    where
        (pats, as, bs) = unzip3 it

-- All this code could probably be simplified by:
-- 
-- 1) Implementing a simpler Pat type with primitive patterns as
--    constructors, each pattern having a Data.Set.Set of "as-bound" variable names,
--    and probably also strictness annotations (?).  The resulting type should be
--    isomorphic to 'Pat'.  There should also be a constructor for patterns that cannot succeed.
--
-- 2) Defining a Boolean lattice on this new pattern type; then, the 'mergePat' 
--    function should be easy to define, something along the lines of:
-- 
--       mergePat (p1, a) (p2, b) = filter canMatch
--           [ (p1 `op` p2, mA, mB) 
--           | (op, mA, mB) <- [ (intersect, Just a, Just b)]
--                             , (diff,      Just a, Nothing)
--                             , (flip diff, Nothing, Just b)
--                             ]
--           ]
-- 
--      where 'diff' is a relative difference operator (which is why the lattice must be boolean)
--      (although actually we may be able to get away with diff = const.  Not sure
--      though, it may be possible for subsequent 'mergePat's to eliminate an intersection case as
--      infeasible and inadvertently expose a 'const' case.  Although I tend to think not,
--      as if the intersection is infeasible then the portion of the const case that overlaps
--      with it should also be infeasible.  I guess a more important concern is that without a true
--      'diff' the 'canMatch' would not prune as much as possible;  in cases where one
--      pattern entirely eclipses the other, or when both match exactly the same cases, the
--      redundant diff cases should be pruned.)

-- Actually, pruning should still be achievable by a 'shadows' predicate:
-- (pruning is then done by "nubBy (flip shadows)")
-- a `shadows` b = (b <= intersect a b)
