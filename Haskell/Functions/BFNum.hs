module Functions.BFNum where

import Prelude hiding (mapM)
import Data.Traversable
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Control.Monad.State (evalState, get, put)

data Tree a
    = E
    | T a (Tree a) (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap f E = E
    fmap f (T x a b) = T (f x) (fmap f a) (fmap f b)

instance Foldable Tree where
    foldMap f E = mempty
    foldMap f (T x a b) = foldMap f a `mappend` f x `mappend` foldMap f b

instance Traversable Tree where
    sequenceA E = pure E
    sequenceA (T x a b) = flip T <$> sequenceA a <*> x <*> sequenceA b

bfNum :: Traversable t => t a -> t Int
bfNum = flip evalState 1 . mapM (const next)
    where
        next = do
            n <- get
            put $! (n+1)
            return n
