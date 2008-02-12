{-# OPTIONS -fth -fglasgow-exts -fno-monomorphism-restriction #-}
{-
 -	"Tree.hs"
 -	(c) 2008 James Cook
 -}

module Tree where

import qualified Fmap
import Cata

import Data.List hiding (insert)
import Control.Monad hiding (join)

data Tree a = Null | Fork a (Tree a) (Tree a)
        deriving (Eq, Show)

isEmpty :: Ord a => Tree a -> Bool
isEmpty Null = True
isEmpty (Fork x a b) = False

minElem :: Ord a => Tree a -> a
minElem (Fork x a b) = x

deleteMin :: Ord a => Tree a -> Tree a
deleteMin (Fork x a b) = merge a b

decompose :: (Ord a) => Tree a -> Maybe (a, Tree a)
decompose  Null         = Nothing
decompose (Fork x a b)  = Just (x, merge a b)

insert :: Ord a => a -> Tree a -> Tree a
insert x a = merge (Fork x Null Null) a

merge :: Ord a => Tree a -> Tree a -> Tree a
merge a Null = a
merge Null b = b
merge a b
   | minElem a <= minElem b = join a b
   | otherwise = join b a

join (Fork x a b) c = Fork x b (merge a c)

instance Functor Tree
        where
                fmap = $(Fmap.fmap ''Tree)

cataList = $(cata ''[])
cataTree = $(cata ''Tree)

anaList = unfoldr
anaTree f b = case f b of
        Just (a, b1, b2)        -> Fork a (anaTree f b1) (anaTree f b2)
        Nothing                 -> Null

hyloTreeList z f g = anaTree g . cataList z f 

hyloListTree z f g = anaList g . cataTree z f

hyloTree :: (a -> Maybe (b, a, a))
         -> d
         -> (b -> d -> d -> d)
         -> a
         -> d
hyloTree f h i = cataTree h i . anaTree f

hyloList z f g = anaList g . cataList z f

hyloList' z f g = unfoldr g . foldr f z

f x = do
        guard (x > 2)
        return (x-1, 0, x-2)

fib = hyloTree f 1 (\a b c -> a + b + c)
sort = hyloTree (\xs -> case xs of [] -> Nothing; (x:xs) -> Just (x, filter (<x) xs, filter (>= x) xs)) [] (\a xs ys -> xs ++ a : ys) 

sort2 :: (Ord a) => [a] -> [a]
sort2 = hyloList Null insert decompose

sort2' :: (Ord a) => [a] -> [a]
sort2' = hyloList' Null insert decompose

-- the 2-level effect would require a bit more work...
data T2 a = Tip2 | Node2 a [T2 a]