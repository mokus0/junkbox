{-# OPTIONS -fglasgow-exts #-}
-- consider: the "Functor" typeclass characterizes covariant endofunctors:
-- 
-- class Covariant f where
--	coMap :: (a -> b) -> f a -> f b
--
-- instance (Functor f) => Covariant f where
--	coMap = fmap

-- Are there any useful instances of Contravariant functors?
-- 
class Contravariant f where
	contraMap :: (a -> b) -> f b -> f a

-- (-> a) is the first instance that comes to mind, but it cannot be directly
-- expressed in Haskell because Haskell doesn't allow "sections" of
-- type constructors.  Instead, we wrap it in a 'newtype', such that the
-- "section" can be replaced with a partial application
newtype Flip f a b = Flip {unFlip :: f b a}

instance Contravariant (Flip (->) a) where
	contraMap f (Flip x) = Flip (x . f)