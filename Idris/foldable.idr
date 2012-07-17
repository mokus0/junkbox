module foldable

class Foldable (f : Set -> Set) where
    fold    : Monoid m => f m -> m
    fold = foldMap id
    foldMap : Monoid m => (a -> m) -> f a -> m

instance Foldable List where
    fold         = foldr (<+>) neutral
    foldMap f xs = fold (map f xs)

instance Foldable Maybe where
    foldMap f = maybe neutral f
