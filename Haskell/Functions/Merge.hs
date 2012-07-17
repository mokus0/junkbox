{-# LANGUAGE FlexibleInstances #-}
module Functions.Merge where

import Control.Applicative
import Control.Monad.ST.Safe
import Data.List
import Data.Monoid
import qualified Data.Vector.Safe as BV
import qualified Data.Vector.Unboxed.Safe as UV
import qualified Data.Vector.Generic.Safe as V
import qualified Data.Vector.Generic.Mutable.Safe as MV
import Test.QuickCheck

mergeBy
  :: (V.Vector v1 a, V.Vector v2 a, V.Vector v3 a) =>
     (a -> a -> Ordering) -> v1 a -> v2 a -> v3 a
mergeBy cmp v1 v2 = V.create $ do
    let n1 = V.length v1
        n2 = V.length v2
    
    v3 <- MV.new (n1 + n2)
    
    let loop i j = case (i < n1, j < n2) of
            (True,  True)  -> accept (case cmp3 l r of LT -> l; _ -> r)
            (True,  False) -> accept l
            (False, True)  -> accept r
            (False, False) -> return v3
            where
                accept (x, i', j') = do
                    MV.write v3 (i+j) x
                    loop i' j'
                l = (v1 V.! i, i+1, j)
                r = (v2 V.! j, i, j+1)
                cmp3 (x1,y1,z1) (x2,y2,z2) = cmp x1 x2 `mappend` compare (y1,z1) (y2,z2)
    
    loop 0 0

merge :: (Ord a, V.Vector v1 a, V.Vector v2 a, V.Vector v3 a) =>
     v1 a -> v2 a -> v3 a
merge = mergeBy compare

newtype Sorted a = Sorted a deriving (Eq, Show)
instance Functor Sorted where fmap f (Sorted x) = Sorted (f x)

instance (Arbitrary a, Ord a) => Arbitrary (Sorted [a]) where
    arbitrary = Sorted . sort <$> arbitrary
instance (Arbitrary a, Ord a) => Arbitrary (Sorted (BV.Vector a)) where
    arbitrary = fmap V.fromList <$> arbitrary
instance (Arbitrary a, Ord a, UV.Unbox a) => Arbitrary (Sorted (UV.Vector a)) where
    arbitrary = fmap V.fromList <$> arbitrary

type Prop_merges_sorted a = Sorted (BV.Vector a) -> Sorted (BV.Vector a) -> Bool

prop_merges_sorted :: (Arbitrary a, Ord a) => Prop_merges_sorted a
prop_merges_sorted (Sorted xs) (Sorted ys)
    =  BV.toList (merge xs ys)
    == sort (BV.toList xs ++ BV.toList ys)
