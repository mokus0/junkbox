{-# LANGUAGE FlexibleContexts #-}
module TypeExperiments.Rope.FTUtils where

import Control.Applicative
import Data.Monoid
import Data.FingerTree              as FT

headM :: (Monad m, Measured v a) => FingerTree v a -> m a
headM ft = case FT.viewl ft of
    x :< _  -> return x
    _       -> fail "ftHead: empty FingerTree"

unsafeMapM :: Monad m => (a -> m b) -> FingerTree v a -> m (FingerTree v b)
unsafeMapM f = unwrapMonad . unsafeTraverse (WrapMonad . f)

splitFingerTreeBy 
    :: (Num n, Ord n, Measured (Sum n) t)
    => (n -> t -> (t, t)) 
    -> n -> FingerTree (Sum n) t -> (FingerTree (Sum n) t, FingerTree (Sum n) t)
splitFingerTreeBy basicUnsafeSplit i ft =
    case compare nFirst i of
        LT -> adjusted 
        EQ -> (first, rest)
        GT -> error "splitFingerTreeBy: this should not happen"
    
    where
        (first, rest) = FT.split (> Sum i) ft
        Sum nFirst = measure first
        
        adjusted = (first |> l, r <| toKeep)
            where
                toSplit :< toKeep = viewl rest
                (l,r) = basicUnsafeSplit (i - nFirst) toSplit

sliceFingerTreeBy
    :: (Num n, Ord n, Measured (Sum n) t)
    => (n -> t -> (t,t))
    -> n -> n -> FingerTree (Sum n) t -> FingerTree (Sum n) t
sliceFingerTreeBy basicUnsafeSplit start len
    = fst . splitFingerTreeBy basicUnsafeSplit len
    . snd . splitFingerTreeBy basicUnsafeSplit start
