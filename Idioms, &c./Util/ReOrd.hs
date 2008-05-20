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

-- some other alternative implementations of order-related functions,
-- not necessarily using the ReOrd type

minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy (*) x y = case x * y of
        GT      -> y
        _       -> x

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy (*) x y = case x * y of
        LT      -> y
        _       -> x


-- minimaBy, maximaBy :: like minimumBy, maximumBy, but return _all_
-- matches.  similarly 'minima', 'maxima'
minimaBy :: (a -> a -> Ordering) -> [a] -> [a]
minimaBy cmp list = minimaBy' [] list
        where   minimaBy' ms []           = ms
                minimaBy' [] (x:xs)       = minimaBy' [x] xs
                minimaBy' ms@(m:_) (x:xs) = case m `cmp` x of
                        LT -> minimaBy' ms      xs
                        EQ -> minimaBy' (x:ms)  xs
                        GT -> minimaBy' [x]     xs

maximaBy :: (a -> a -> Ordering) -> [a] -> [a]
maximaBy cmp list = maximaBy' [] list
        where   maximaBy' ms []           = ms
                maximaBy' [] (x:xs)       = maximaBy' [x] xs
                maximaBy' ms@(m:_) (x:xs) = case m `cmp` x of
                        LT -> maximaBy' [x]     xs
                        EQ -> maximaBy' (x:ms)  xs
                        GT -> maximaBy' ms      xs


minima, maxima :: (Ord a) => [a] -> [a]
minima = minimaBy compare
maxima = maximaBy compare