{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances
  #-}
module Math.InverseInterpRootFinder where

-- An interesting idea, but not at very robust at all.
-- Somewhat inspired by my limited understanding of why Brent's method works.
-- I suspect the major deficiency here is in the realm of bookkeeping.  When
-- Order=3 I would like Brent's method to be a special case of this one.
-- I think the choice of which points to keep is presently not very good.
-- Specifically, it's keeping things only based on their 'f' coords, so it
-- can inadvertently keep bad X's if 'f' happens to be very low there.  Once
-- that happens, there's really no chance of recovery other than to bisect until
-- we hit a region with small enough f values to roll the bad ones off.
-- Also, the order of 'takeMonotonic' should be related between both sides.
-- That's another place bad guesses can derail the whole thing - if the 1st 
-- or 2nd element of the list is bad, the rest will be mistakenly rejected.

import NR.Ch3.S1
import NR.Ch3.S2
import NR.Ch9.S1
import Data.List
import qualified Data.Vector as V

data InverseInterp a b 
    = Converged !a
    | InverseInterp
        { over      :: [(b,a)]
        , under     :: [(b,a)]
        , slow      :: !Bool
        } deriving (Eq, Show)

order = 10
n = (order `div` 2)
m = order - n

addPoint xNew fNew slow (InverseInterp over under _) =
    case compare fNew 0 of
        LT -> InverseInterp (take n over) (takeMonotonic $ insertBy (flip compare) (fNew, xNew) $ take (m-1) under) slow
        EQ -> Converged xNew
        GT -> InverseInterp (takeMonotonic $ insert (fNew, xNew) $ take (m-1) over) (take n under) slow

instance (Fractional a, Ord a) => RootFinder InverseInterp a a where
    initRootFinder f x1 x2
        | f1 > 0 && f2 < 0  = InverseInterp [(f1,x1)] [(f2,x2)] False
        | f1 < 0 && f2 > 0  = InverseInterp [(f2,x2)] [(f1,x1)] False
        | f1 == 0           = Converged x1
        | f2 == 0           = Converged x2
        | otherwise         = error "InverseInterp: interval does not bracket a root"
        where f1 = f x1; f2 = f x2
    stepRootFinder f orig@Converged{} = orig
    stepRootFinder f orig@(InverseInterp over under _)
        | abs ((xInterp - xMid) / xRange) > 0.4
        || accept xInterp False == orig               = accept xMid True
        | otherwise                             = accept xInterp False
        where
            table = takeMonotonic (reverse under ++ over)
            xInterp = fst (polyRawInterp (V.fromList table) 0 0)
            xMid    = 0.5 * (x0+x1)
            xRange  = x1 - x0
            
            x0 = min xOver xUnder
            x1 = max xOver xUnder
            
            -- (x0, x1)
            --     | xOver > xUnder    = (minimum (map snd over), maximum (map snd under))
            --     | otherwise         = (maximum (map snd over), minimum (map snd under))
            
            xUnder = snd (head under)
            xOver  = snd (head over )
            
            accept xNew slow = let fNew = f xNew in addPoint xNew fNew slow orig
    
    estimateRoot (Converged x) = x
    estimateRoot (InverseInterp ((_,a):_) ((_,b):_) _) = 0.5 * (a+b)
    estimateError (Converged x) = 0
    estimateError (InverseInterp ((_,a):_) ((_,b):_) _) = 0.5 * (b-a)

takeMonotonic :: (Eq a, Ord b) => [(a,b)] -> [(a,b)]
takeMonotonic [x] = [x]
takeMonotonic  (hd@(x,y0):rest@((_,y1):_))
    | dir == EQ = [hd] -- shouldn't be possible here
    | otherwise = hd : go x y0 rest
    where
        dir = compare y0 y1
        
        go _ _ [] = []
        go x0 y0 (hd@(x1,y1):rest)
            | x0 == x1              = []
            | compare y0 y1 == dir  = hd : go x1 y1 rest
            | otherwise             = []