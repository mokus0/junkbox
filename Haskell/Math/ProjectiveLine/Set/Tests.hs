{-# LANGUAGE NoMonomorphismRestriction #-}
module Math.ProjectiveLine.Set.Tests where

import Math.ProjectiveLine
import Math.ProjectiveLine.Set

import Control.Applicative hiding (empty)
import Test.QuickCheck

newtype PrimSet a = PrimSet { unPrimSet :: (Set a) }

instance Arbitrary a => Arbitrary (ProjectiveLine a) where
    arbitrary = frequency
        [ (1,  pure Infinity)
        , (50, Real <$> arbitrary)
        ]

instance (Arbitrary a, Ord a) => Arbitrary (PrimSet a) where
    arbitrary = PrimSet <$> frequency
            [ (1,  pure empty)
            , (1,  pure full)
            , (10, singleton   <$> arbitrary)
            , (10, cosingleton <$> arbitrary)
            , (30, uncurry range <$> (arbitrary `suchThat` isSaneRange))
            ]
        where
            isSaneRange (a,b) = (fst a /= fst b) || (snd a == snd b)

instance (Arbitrary a, Real a) => Arbitrary (Set a) where
    arbitrary = sized $ \sz -> frequency
        [ (2, unPrimSet <$> arbitrary)
        , (1,  elements [complement] <*> arbitrary)
        , (,) sz $ do
            sz' <- choose (1,sz-1)
            let a = max 1 sz'; b = max 1 (sz-sz')
            xor <$> resize a arbitrary
                <*> resize b arbitrary
        ]

prop_setOp_reflects_binOp setOp binOp s1 s2 x
    =  x `member` (setOp s1 s2)
    == binOp (x `member` s1) (x `member` s2)

prop_union_def      = prop_setOp_reflects_binOp union (||)
prop_intersect_def  = prop_setOp_reflects_binOp intersect (&&)
prop_difference_def = prop_setOp_reflects_binOp difference (\x y -> x && not y)

prop_preserves_validity op s = 
    valid s ==> valid (op s)
prop_preserves_validity2 op s1 s2 = 
    valid s1 && valid s2 ==> valid (op s1 s2)
