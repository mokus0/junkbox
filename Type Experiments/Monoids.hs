{-
 -      ``Monoids''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Monoids where

import Data.Monoid
import Data.Unamb
import Control.Monad

data Max a = Max (Maybe a) deriving (Eq, Show)

instance Ord a => Monoid (Max a) where
        mempty = Max (Nothing)
        mappend a b = unamb (mx a b) (mx b a)
                where 
                        mx a (Max Nothing) = a
                        mx (Max (Just a)) (Max (Just b)) = Max (Just (max a b))
