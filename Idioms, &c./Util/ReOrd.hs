{-
 -      ``Util/ReOrd.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Util.ReOrd where

newtype Ord a => OrdRev a = OrdRev a
        deriving (Eq, Show)

instance (Ord a) => Ord (OrdRev a) where
        compare (OrdRev a) (OrdRev b) = compare b a

data ReOrd a = ReOrd { cmp   :: a -> a -> Ordering
                     , item  :: a
                     }

instance Eq (ReOrd a) where
        a == b  = case cmp a (item a) (item b) of
                EQ -> True
                __ -> False
        a /= b  = case cmp a (item a) (item b) of
                EQ -> False
                __ -> True

instance Ord (ReOrd a) where
        compare a b = cmp a (item a) (item b)