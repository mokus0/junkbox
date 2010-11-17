module Math.Quat where

import Data.List
import Data.Ord

data Quat a = Quat a a a a
    deriving (Eq, Show)

instance (Floating a, Ord a) => Num (Quat a) where
    fromInteger a = Quat (fromInteger a) 0 0 0
    negate (Quat x y z w) = Quat (-x) (-y) (-z) (-w)
    abs q = Quat (magnitude q) 0 0 0
    signum q@(Quat x y z w) = Quat (x/mag) (y/mag) (z/mag) (w/mag)
        where mag = magnitude q

    Quat a1 b1 c1 d1 + Quat a2 b2 c2 d2 = Quat (a1+a2) (b1+b2) (c1+c2) (d1+d2)
    Quat a1 b1 c1 d1 - Quat a2 b2 c2 d2 = Quat (a1-a2) (b1-b2) (c1-c2) (d1-d2)
    Quat a1 b1 c1 d1 * Quat a2 b2 c2 d2 = Quat (ssum [a1*a2, - b1*b2, - c1*c2, - d1*d2])
                                               (ssum [a1*b2,   b1*a2,   c1*d2, - d1*c2])
                                               (ssum [a1*c2, - b1*d2,   c1*a2,   d1*b2])
                                               (ssum [a1*d2,   b1*c2, - c1*b2,   d1*a2])
        
magnitude q = sqrt (magnitudeSq q)

magnitudeSq (Quat x y z w) = ssum [x*x, y*y, z*z, w*w]

ssum [] = 0
ssum [x] = x
ssum xs = case sortBy (comparing abs) xs of
    (a:b:rest) -> ssum (a+b : rest)

scale k (Quat a b c d) = Quat (k*a) (k*b) (k*c) (k*d)