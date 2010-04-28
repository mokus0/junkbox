{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module NR.Ch9.S2 where

import NR.Ch9.S1
import Data.List
import Data.Ord

-- | Using the false-position method, return a root of a function known
-- to lie between x1 and x2.  The root is refined until its accuracy is += xacc.
rtflsp f x1 x2 xacc = 
    (estimateRoot :: (Fractional a, Ord a) => FalsePosition a a -> a)
    (findRoot f x1 x2 xacc)
    
-- |Iteratively refine a bracketing interval [x1, x2] of a root of f
-- until total convergence (which may or may not ever be achieved) using 
-- the false-position method.
data FalsePosition a b = FalsePosition
    { fpRoot :: !a
    , fpDX   :: !a
    , fpXL   :: !a
    , fpFL   :: !a
    , fpXH   :: !a
    , fpFH   :: !a
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

-- |Using the secant method, return the root of a function thought to lie between
-- x1 and x2.  The root is refined until its accuracy is +-xacc.
rtsec f x1 x2 xacc = 
    (estimateRoot :: (Fractional a, Ord a) => SecantMethod a a -> a)
    (findRoot f x1 x2 xacc)

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
        { secDX
        , secXL   :: !a
        , secFL   :: !b
        , secRTS  :: !a
        , secFRTS :: !b
        } deriving (Eq, Show)

instance (Fractional a, Ord a) => RootFinder SecantMethod a a where
    initRootFinder f x1 x2
        | abs f1 < abs f2       = stepRootFinder f $ SecantMethod 0 x2 f2 x1 f1
        | otherwise             = stepRootFinder f $ SecantMethod 0 x1 f1 x2 f2
        where f1 = f x1; f2 = f x2
    
    stepRootFinder f orig@ConvergedSecantMethod{} = orig
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

zriddr f x1 x2 xacc =
    (estimateRoot :: (Floating a, Ord a) => RiddersMethod a a -> a)
    (findRoot f x1 x2 xacc)

data RiddersMethod a b
    = ConvergedRidders !a
    | RiddersMethod
        { ridXL     :: !a
        , ridFL     :: !b
        , ridXH     :: !a
        , ridFH     :: !b
        } deriving (Eq, Show)

instance (Floating a, Ord a) => RootFinder RiddersMethod a a where
    initRootFinder f x1 x2
        |  f1 < 0 && f2 < 0
        || f2 > 0 && f1 > 0 = error "riddersMethod: interval does not bracket a root"
        | otherwise         = RiddersMethod x1 f1 x2 f2
        where
            f1 = f x1
            f2 = f x2
    stepRootFinder f orig@ConvergedRidders{} = orig
    stepRootFinder f (RiddersMethod xl fl xh fh)
            | signNEQ fm fNew   = finish xNew fNew xm fm
            | signNEQ fl fNew   = finish xNew fNew xl fl
            | signNEQ fh fNew   = finish xNew fNew xh fh
            | otherwise         = error "RiddersMethod: encountered singularity"
            where
                xm = 0.5 * (xl + xh)
                fm = f xm
                s = sqrt (fm*fm - fl*fh)
                xNew = xm + (xm-xl)*((if fl >= fh then id else negate) fm / s)
                fNew = f xNew
                
                signNEQ a b = a /= 0 && signum b /= signum a
                
                finish xl fl xh fh
                    | xl == xh  = ConvergedRidders xl
                    | fl == 0   = ConvergedRidders xl
                    | fh == 0   = ConvergedRidders xh
                    | otherwise = RiddersMethod xl fl xh fh
    
    estimateRoot (ConvergedRidders x)       = x
    estimateRoot RiddersMethod{ridXL = x}  = x
    
    estimateError ConvergedRidders{}        = 0
    estimateError RiddersMethod{ridXL = xl, ridXH = xh} = xl - xh
