-- hacking around with ideas from the first few lines of Oleg Kiselyov's zip-folds paper
-- (http://okmij.org/ftp/Haskell/zip-folds.lhs)
-- I have not read it in full at time of coding; I want to see what I can come up with.
{-# LANGUAGE Rank2Types #-}

module TypeExperiments.FoldTransform where

import Prelude hiding ((++), concat, head, tail, null)
import qualified Prelude as P

newtype Fold a = Fold {fold :: (forall ans. (a -> ans -> ans) -> ans -> ans)}

fromString = fromList :: String -> Fold Char
fromList l = foldr cons nil l
toList (Fold f) = f (:) []

test f = f . fromList

cons x xs = Fold (\cons nil -> cons x (fold xs cons nil))
nil = Fold (\cons nil -> nil)

null (Fold x) = x (\a b -> False) True

x ++ y = Fold (\cons nil -> fold x cons (fold y cons nil))
concat xs = Fold (\cons nil -> fold xs (\x rest -> fold x cons rest) nil)

instance Show a => Show (Fold a) where
    showsPrec p = showsPrec p . toList

instance Functor Fold where
    fmap f (Fold x) = Fold (\cons -> x (cons . f))

instance Monad Fold where
    return x = Fold (\cons nil -> cons x nil)
    x >>= f = concat (fmap f x)

head (Fold x) = x (\x _ -> Just x) Nothing -- (error "head: empty list")
tail (Fold x) = Fold (\cons nil -> x (\e r f -> f (cons e (r f)) (r true)) (\f -> f nil (error "tail: empty list")) false)

false t f = f
true  t f = t