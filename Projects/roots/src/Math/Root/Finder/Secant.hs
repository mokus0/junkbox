{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Math.Root.Finder.Secant
    ( SecantMethod, secant
    ) where

import Math.Root.Finder

-- |Using the secant method, return the root of a function thought to lie between
-- x1 and x2.  The root is refined until its accuracy is +-xacc.
secant :: (Ord a, Fractional a) => (a -> a) -> a -> a -> a -> Either (SecantMethod a a) a
secant f x1 x2 xacc = fmap estimateRoot (findRoot f x1 x2 xacc)

-- |Iteratively refine 2 estimates x1, x2 of a root of f until total 
-- convergence (which may or may not ever be achieved) using the
-- secant method.
-- 
-- Each element of the returned list is a pair (x,dx) where
-- x is a new estimate of a root of f x and dx is the amount this
-- estimate differs from the last.

data SecantMethod a b
    = ConvergedSecantMethod !a
    | SecantMethod
        { secDX    :: !a
        , secXL    :: !a
        , _secFL   :: !b
        , _secRTS  :: !a
        , _secFRTS :: !b
        } deriving (Eq, Show)

instance (Fractional a, Ord a) => RootFinder SecantMethod a a where
    initRootFinder f x1 x2
        | abs f1 < abs f2       = stepRootFinder f $ SecantMethod 0 x2 f2 x1 f1
        | otherwise             = stepRootFinder f $ SecantMethod 0 x1 f1 x2 f2
        where f1 = f x1; f2 = f x2
    
    stepRootFinder _ orig@ConvergedSecantMethod{} = orig
    stepRootFinder f (SecantMethod _ xl fl rts fRts)
        | fNew == 0 = ConvergedSecantMethod xNew
        | otherwise = SecantMethod dx rts fRts xNew fNew
        where
            dx = (xl - rts) * fRts / (fRts - fl)
            xNew = rts + dx
            fNew = f xNew
    
    estimateRoot (ConvergedSecantMethod x)  = x
    estimateRoot SecantMethod{secXL = x}    = x
    
    estimateError ConvergedSecantMethod{}   = 0
    estimateError SecantMethod{secDX = dx}  = dx
