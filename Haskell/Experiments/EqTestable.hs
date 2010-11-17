{-# LANGUAGE FlexibleInstances #-}
module Experiments.EqTestable where

import Test.QuickCheck

instance Eq a => Testable (a,a) where
    property (x,y) = property (x == y)

instance Ord a => Testable (a, Ordering, a) where
    property (x, ord, y) = property (compare x y == ord)

prop_associative (#) a b c = ((a # b) # c ,  a # (b # c))
prop_commutative (#) a b   = (a # b, b # a)

prop_strictly_monotone f x y = (f x, compare x y, f y)
