{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Math.Root.Finder.Bisection
    ( Bisect, bisection
    ) where

import Math.Root.Finder

-- |Bisect an interval in search of a root.  At all times, @f (estimateRoot _)@
-- is less than or equal to 0 and @f (estimateRoot _ + estimateError _)@ is 
-- greater than or equal to 0.
data Bisect a b = Bisect { _bisX :: !a, _bisF :: !b , _bisDX :: !a }
    deriving (Eq, Ord, Show)

instance (Fractional a, Ord b, Num b) => RootFinder Bisect a b where
    initRootFinder f x1 x2
        | f1 < 0    = Bisect x1 f1 (x2-x1)
        | otherwise = Bisect x2 f2 (x1-x2)
        where f1 = f x1; f2 = f x2
    
    stepRootFinder f orig@(Bisect x fx dx) = case fMid `compare` 0 of
            LT ->  Bisect xMid fMid dx2
            EQ ->  orig
            GT ->  Bisect x fx dx2 
            where
                dx2 = dx * 0.5
                xMid = x + dx2
                fMid = f xMid
    
    estimateRoot  (Bisect x _  _) = x
    
    estimateError (Bisect _ 0  _) = 0
    estimateError (Bisect _ _ dx) = dx

-- |Using bisection, return a root of a function known to lie between x1 and x2.
-- The root will be refined till its accuracy is +-xacc.
bisection :: (Ord a, Fractional a, Ord b, Num b) => (a -> b) -> a -> a -> a -> Either (Bisect a b) a
bisection f x1 x2 xacc = fmap estimateRoot (findRoot f x1 x2 xacc)
