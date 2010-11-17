{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
import Data.Maybe(fromJust)

lk x = flip lookup x
flipPair = uncurry $ flip (,)

class (Eq a, Eq b) => Convert a b where
   mapping :: [(a, b)]
   mapping = error "No mapping defined"
   convert :: a -> b
   convert = fromJust . lk mapping

instance (Convert a b) => Convert b a where
   convert = fromJust . lk (map flipPair mapping)

data XX = One | Two | Three deriving (Show, Eq)
data YY = Eno | Owt | Eerht deriving (Show, Eq)

instance Convert XX YY where
   mapping = [(One, Eno), (Two, Owt), (Three, Eerht)]

fromXX :: Convert XX a => XX -> a
fromXX = convert

fromXX' :: XX -> a
fromXX' = convert

main = do
    print (fromXX  One :: YY)
    print (fromXX' Two :: YY)
