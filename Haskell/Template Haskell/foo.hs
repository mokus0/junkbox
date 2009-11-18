{-# OPTIONS -fno-monomorphism-restriction #-}

module Main where

import Data.List (unfoldr)

-- Is there a term for this type of transformation?
-- it's essentially a list hylomorphism in reverse

notHylo z f g = unfoldr g . foldr f z

-- an example of it in action:
-- (skew-heap sorting)

data Tree a = Null | Fork a (Tree a) (Tree a)
        deriving (Eq, Show)


top :: Ord a => Tree a -> a
top (Fork x a b) = x

push :: Ord a => a -> Tree a -> Tree a
push x a = merge (Fork x Null Null) a

merge :: Ord a => Tree a -> Tree a -> Tree a
merge a Null = a
merge Null b = b
merge a b
   | top a <= top b     = splice a b
   | otherwise          = splice b a

splice :: (Ord a) => Tree a -> Tree a -> Tree a
splice (Fork x a b) c = Fork x b (merge a c)

pop :: (Ord a) => Tree a -> Maybe (a, Tree a)
pop  Null         = Nothing
pop (Fork x a b)  = Just (x, merge a b)

skewSort = notHylo Null push pop

main = print $ length $ skewSort [1..1000000]