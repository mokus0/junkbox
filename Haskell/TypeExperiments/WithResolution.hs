{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module TypeExperiments.WithResolution (withResolution, withDecimals, withRadixPlaces, atRes) where

import Data.Fixed

data X10 a = X10
data X a b = X

data P1 a = P1
data P2 a = P2
data P3 a = P3
data P4 a = P4
data P5 a = P5
data P6 a = P6
data P7 a = P7
data P8 a = P8
data P9 a = P9

-- some 'common' base resolutions
data R0  = R0
data R1  = R1
data R2  = R2
data R4  = R4
data R8  = R8
data R10 = R10
data R16 = R16

instance HasResolution R0  where resolution _ = 0
instance HasResolution R1  where resolution _ = 1
instance HasResolution R2  where resolution _ = 2
instance HasResolution R4  where resolution _ = 4
instance HasResolution R8  where resolution _ = 8
instance HasResolution R10 where resolution _ = 10
instance HasResolution R16 where resolution _ = 16

resolutionOf :: HasResolution a => g (f a) -> Integer
resolutionOf = resolution . f
    where
        f :: g (f a) -> f a
        f = undefined

instance HasResolution a => HasResolution (P1  a) where resolution a = resolutionOf a + 1
instance HasResolution a => HasResolution (P2  a) where resolution a = resolutionOf a + 2
instance HasResolution a => HasResolution (P3  a) where resolution a = resolutionOf a + 3
instance HasResolution a => HasResolution (P4  a) where resolution a = resolutionOf a + 4
instance HasResolution a => HasResolution (P5  a) where resolution a = resolutionOf a + 5
instance HasResolution a => HasResolution (P6  a) where resolution a = resolutionOf a + 6
instance HasResolution a => HasResolution (P7  a) where resolution a = resolutionOf a + 7
instance HasResolution a => HasResolution (P8  a) where resolution a = resolutionOf a + 8
instance HasResolution a => HasResolution (P9  a) where resolution a = resolutionOf a + 9
instance HasResolution a => HasResolution (X10 a) where resolution a = resolutionOf a * 10

instance (HasResolution a, HasResolution b) => HasResolution (X a b) 
    where resolution a = resolution (x1 a) * resolution (x2 a)
            where
                x1 :: f (X a b) -> f a
                x1 = undefined
                x2 :: f (X a b) -> f b
                x2 = undefined

-- |Construct and provide a resolution with the given resolution, for use with 'Fixed'
withResolution :: Integer -> (forall a. HasResolution a => a -> b) -> b
withResolution 0  f = f R0
withResolution 1  f = f R1
withResolution 2  f = f R2
withResolution 4  f = f R4
withResolution 8  f = f R8
withResolution 10 f = f R10
withResolution 16 f = f R16
withResolution n f = case n `divMod` 10 of
    (d,0) -> withResolution d (\x -> f (appRes X10 x))
    (d,1) -> withResolution d (\x -> f (appRes P1 (appRes X10 x)))
    (d,2) -> withResolution d (\x -> f (appRes P2 (appRes X10 x)))
    (d,3) -> withResolution d (\x -> f (appRes P3 (appRes X10 x)))
    (d,4) -> withResolution d (\x -> f (appRes P4 (appRes X10 x)))
    (d,5) -> withResolution d (\x -> f (appRes P5 (appRes X10 x)))
    (d,6) -> withResolution d (\x -> f (appRes P6 (appRes X10 x)))
    (d,7) -> withResolution d (\x -> f (appRes P7 (appRes X10 x)))
    (d,8) -> withResolution d (\x -> f (appRes P8 (appRes X10 x)))
    (d,9) -> withResolution d (\x -> f (appRes P9 (appRes X10 x)))

appRes :: f a -> a -> f a
appRes a b = a

-- |Construct and provide a resolution with the given number of decimal places, for use with 'Fixed'
withDecimals :: Integer -> (forall a. HasResolution a => a -> b) -> b
withDecimals 0 f      = f R1
withDecimals (n+1) f  = withDecimals n (\x -> f (app X10 x))
    where
        app :: (f a) -> a -> (f a)
        app = undefined

-- |@withRadixPlaces b n@ provides its argument function with a resolution constructed
-- to have @n@ base-@b@ places in its fractional part.
withRadixPlaces :: Integer -> Integer -> (forall a. HasResolution a => a -> b) -> b
withRadixPlaces b n (f :: forall a. HasResolution a => a -> b) = withResolution b (go n R1)
    where
        go :: (HasResolution r, HasResolution x) => Integer -> r -> x -> b
        go  0    x r = f x
        go (n+1) x r = go n (mulRes r x) r
        
        mulRes :: s -> t -> X s t
        mulRes r x = undefined

-- |Constrain a 'Fixed' value to have the specified resolution.
atRes :: HasResolution r => r -> Fixed r -> Fixed r
atRes r x = x