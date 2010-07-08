{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies #-}
module TypeExperiments.Ord where

import Prelude hiding (Ord(..))

data Default = Default deriving (Eq, Show)

class PartialOrd o a where
    pCompare :: o -> a -> a -> Maybe Ordering
    eq   :: o -> a -> a -> Bool
    eq o a b = case pCompare o a b of
        Just EQ -> True
        _       -> False
    ineq :: o -> a -> a -> Bool
    ineq o a b = case pCompare o a b of
        Just LT -> True
        Just GT -> True
        _ -> False
    inc  :: o -> a -> a -> Bool
    inc o a b = case pCompare o a b of
        Just _  -> False
        Nothing -> True
    lt   :: o -> a -> a -> Bool
    lt o a b = pCompare o a b == Just LT
    lte  :: o -> a -> a -> Bool
    lte o a b = case pCompare o a b of
        Just LT -> True
        Just EQ -> True
        _ -> False
    gt   :: o -> a -> a -> Bool
    gt o a b = pCompare o a b == Just GT
    gte  :: o -> a -> a -> Bool
    gte o a b = case pCompare o a b of
        Just GT -> True
        Just EQ -> True
        _ -> False

class PartialOrd o a => Ord o a where
    compare :: o -> a -> a -> Ordering

a > b   = compare Default a b == GT
a < b   = compare Default a b == LT
a >= b  = case compare Default a b of
    GT -> True
    EQ -> True
    _  -> False
a <= b  = case compare Default a b of
    LT -> True
    EQ -> True
    _  -> False

sort :: PartialOrd Default a => [a] -> [a]
sort = undefined

sortBy :: PartialOrd o a => o -> [a] -> [a]
sortBy = undefined

newtype Comparing b a = Comparing (a -> b)
data ComparingBy o b a = ComparingBy o (a -> b)
newtype Comparison a = Comparison (a -> a -> Ordering)
newtype PComparison a = PComparison (a -> a -> Maybe Ordering)

instance (PartialOrd Default c, a ~ b) => PartialOrd (Comparing c b) a where
    pCompare (Comparing f) a b = pCompare Default (f a) (f b)

instance (Ord Default c, a ~ b) => Ord (Comparing c b) a where
    compare (Comparing f) a b = compare Default (f a) (f b)

instance (PartialOrd o a, b ~ c) => PartialOrd (ComparingBy o a b) c where
    pCompare (ComparingBy o f) a b = pCompare o (f a) (f b)

instance (Ord o a, b ~ c) => Ord (ComparingBy o a b) c where
    compare (ComparingBy o f) a b = compare o (f a) (f b)

instance a ~ b => PartialOrd (PComparison a) b where
    pCompare (PComparison c) = c

instance a ~ b => PartialOrd (Comparison a) b where
    pCompare (Comparison c) a b = Just (c a b)

instance a ~ b => Ord (Comparison a) b where
    compare (Comparison c) = c