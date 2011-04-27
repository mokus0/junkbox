{-# LANGUAGE NoMonomorphismRestriction #-}
module Math.ProjectiveLine.Set.Tests where

import Math.ProjectiveLine
import Math.ProjectiveLine.Set

import Control.Applicative hiding (empty)
import Data.List (sort)
import Test.QuickCheck
import Data.Bits (shiftR)

instance Arbitrary a => Arbitrary (ProjectiveLine a) where
    arbitrary = frequency
        -- Infinity should have roughly the same frequency as 0
        [ (1,  pure Infinity)
        , (15, Real <$> arbitrary)
        ]

instance (Arbitrary a, Real a) => Arbitrary (Set a) where
    arbitrary = do
        xs <- sort <$> arbitrary
        
        let arbitrarySetFromList xs l
                | l <= 10   = scan empty xs
                | otherwise = do
                    let l0 = l `shiftR` 1
                        l1 = l - l0
                        (xs0, xs1) = splitAt l0 xs
                    s0 <- arbitrarySetFromList xs0 l0
                    s1 <- arbitrarySetFromList xs1 l1
                    return (union s0 s1) 
            
            scan accum []   = return accum
            scan accum xs = oneof $ concat
                [ [ scan (union accum (singleton x)) rest
                  | x:rest <- pure xs
                  ]
                , [ do
                     (inc0, inc1) <- arbitrary
                     scan (union accum (range (x0,inc0) (x1,inc1))) rest
                  | x0:x1:rest <- pure xs
                  , x0 /= x1
                  ]
                ]
        
        set <- arbitrarySetFromList xs (length xs)
        elements [set, complement set]

prop_setOp_reflects_binOp setOp binOp s1 s2 x
    =  x `member` (setOp s1 s2)
    == binOp (x `member` s1) (x `member` s2)

prop_union_def      = prop_setOp_reflects_binOp union       (||)
prop_intersect_def  = prop_setOp_reflects_binOp intersect   (&&)
prop_difference_def = prop_setOp_reflects_binOp difference  (//)
    where x // y = x && not y

prop_preserves_validity op s = 
    valid s ==> valid (op s)
prop_preserves_validity2 op s1 s2 = 
    valid s1 && valid s2 ==> valid (op s1 s2)
