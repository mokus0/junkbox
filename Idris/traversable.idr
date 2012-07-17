module traversable

import foldable

class (Functor t, Foldable t) => Traversable (t : Set -> Set) where
    traverse : Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f t = sequenceA (fmap f t)
    
    sequenceA : Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id

instance Traversable List where
    sequenceA = foldr (\x => \xs => [| x :: xs |]) (pure [])

instance Traversable Maybe where
    sequenceA = maybe (pure Nothing) (\x => [| Just x |])

