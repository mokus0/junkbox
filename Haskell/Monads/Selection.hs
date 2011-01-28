module Monads.Selection where

import Control.Monad

newtype Select r t = Select { select :: (t -> r) -> t }

instance Functor (Select r) where
    fmap f (Select g) = Select (\h -> (f . g) (h . f))

instance Monad (Select r) where
    return x = Select (const x)
    x >>= f  = Select (\h -> select (f (select x (h . flip select h . f))) h)

type Set = Select Bool

set :: [a] -> Set a
set []       = error "set: empty!"
set xs@(x:_) = Select (\p -> head (filter p xs ++ [x]))

union :: Set a -> Set a -> Set a
union x y = Select (\p -> let xp = select x p in if p xp then xp else select y p)

restrict :: (a -> Bool) -> Set a -> Set a
restrict p (Select s) = Select (\q -> s (\x -> p x && q x))

intersection :: Eq a => Set a -> Set a -> Set a
intersection x y = Select (\p -> select x (\x -> p x && contains y (x == )))

contains :: Select r t -> (t -> r) -> r
contains x p = p (select x p)

(!) :: Set t -> (t -> Bool) -> Maybe t
s ! p = if p x then Just x else Nothing
    where x = select s p

