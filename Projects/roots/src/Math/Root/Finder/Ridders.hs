{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Math.Root.Finder.Ridders
    ( RiddersMethod, ridders
    ) where

import Math.Root.Finder

ridders :: (Ord a, Floating a) => (a -> a) -> a -> a -> a -> Either (RiddersMethod a a) a
ridders f x1 x2 xacc = fmap estimateRoot (findRoot f x1 x2 xacc)

data RiddersMethod a b
    = ConvergedRidders !a
    | RiddersMethod
        { ridXL     :: !a
        , _ridFL    :: !b
        , ridXH     :: !a
        , _ridFH    :: !b
        } deriving (Eq, Show)

instance (Floating a, Ord a) => RootFinder RiddersMethod a a where
    initRootFinder f x1 x2
        |  f1 < 0 && f2 < 0
        || f2 > 0 && f1 > 0 = error "riddersMethod: interval does not bracket a root"
        | otherwise         = RiddersMethod x1 f1 x2 f2
        where
            f1 = f x1
            f2 = f x2
    stepRootFinder _ orig@ConvergedRidders{} = orig
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
