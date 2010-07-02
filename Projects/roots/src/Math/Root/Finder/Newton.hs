{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Math.Root.Finder.Newton
    ( Newton, newton
    ) where

import Math.Root.Finder

data Newton a b = Newton
    { newtRTN   :: !a
    , newtDX    :: a
    } deriving (Eq, Show)

instance Fractional a => RootFinder Newton a (a,a) where
    initRootFinder f x1 x2 = stepRootFinder f (Newton rtn undefined)
        where
            rtn = 0.5 * (x1 + x2)
    
    stepRootFinder f Newton{newtRTN = rtn} = Newton (rtn - dx) dx
        where
            (y,dy) = f rtn
            dx = y / dy
    
    estimateRoot Newton{newtRTN = rtn} = rtn
    estimateError Newton{newtDX = dx}  = dx    

-- | Using Newton's method, return a root of a function known
-- to lie between x1 and x2.  The root is refined until its accuracy is += xacc.
newton :: (Ord a, Fractional a) => (a -> (a, a)) -> a -> a -> a -> Either (Newton a (a,a)) a
newton f x1 x2 xacc = fmap estimateRoot (findRoot f x1 x2 xacc)
