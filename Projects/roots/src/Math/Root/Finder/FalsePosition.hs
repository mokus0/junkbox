{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Math.Root.Finder.FalsePosition
    ( FalsePosition, falsePosition
    ) where

import Math.Root.Finder

-- | Using the false-position method, return a root of a function known
-- to lie between x1 and x2.  The root is refined until its accuracy is += xacc.
falsePosition :: (Ord a, Fractional a) => (a -> a) -> a -> a -> a -> Either (FalsePosition a a) a
falsePosition f x1 x2 xacc = fmap estimateRoot (findRoot f x1 x2 xacc)

-- |Iteratively refine a bracketing interval [x1, x2] of a root of f
-- until total convergence (which may or may not ever be achieved) using 
-- the false-position method.
data FalsePosition a b = FalsePosition
    { fpRoot :: !a
    , fpDX   :: !a
    , _fpXL  :: !a
    , _fpFL  :: !a
    , _fpXH  :: !a
    , _fpFH  :: !a
    } deriving (Eq, Show)

instance (Fractional a, Ord a) => RootFinder FalsePosition a a where
    initRootFinder f x1 x2
        -- step once to compute first estimate
        |  f1 <= 0 && f2 >= 0
        || f2 <= 0 && f1 >= 0   = stepRootFinder f $ FalsePosition 0 0 x2 f2 x1 f1
        | otherwise             = error "FalsePosition: given interval does not bracket a root"
        where
            f1 = f x1
            f2 = f x2
    
    stepRootFinder f orig@(FalsePosition _ _ xl fl xh fh) = case compare fNew 0 of
        LT -> FalsePosition xNew (xl - xNew) xNew fNew  xh   fh
        EQ -> orig
        GT -> FalsePosition xNew (xh - xNew) xl   fl    xNew fNew
        where
            dx = xh - xl
            xNew = xl + dx * fl / (fl - fh)
            fNew = f xNew
    
    estimateRoot = fpRoot
    estimateError = fpDX
