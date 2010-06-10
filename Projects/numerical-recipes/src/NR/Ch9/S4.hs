{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module NR.Ch9.S4 where

import NR.Ch9.S1

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
