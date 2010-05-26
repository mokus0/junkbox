{-# LANGUAGE GADTs #-}
module TypeExperiments.GCompare where

import TypeExperiments.Uniq
import Unsafe.Coerce
import Control.Monad.Primitive
import Data.Word

data GOrdering a b where
    GLT :: GOrdering a b
    GEQ :: GOrdering t t
    GGT :: GOrdering a b

-- |Type class for comparable GADT-like structures.  When 2 things are equal,
-- must return a type-level witness that this is so (GEQ).
class GCompare f where
    gcompare :: f a -> f b -> GOrdering a b

geq :: GCompare f => f a -> f b -> Maybe (GOrdering a b)
geq a b = case gcompare a b of
    GEQ -> Just GEQ
    _   -> Nothing

-- |A super-special ad-hoc GADT-like tihng. 'Tag's can be generated in any 
-- primitive monad (but only tags from the same one can be compared).  Every
-- tag is equal to itself and to no other.  The GOrdering class allows 
-- rediscovery of a tag's phantom type, useful for example with the "Env"
-- module in this same dir.
data Tag s a where
    Tag :: Uniq s -> Tag s a
instance GCompare (Tag s) where
    gcompare (Tag a) (Tag b) = case compare a b of
        LT -> GLT
        EQ -> unsafeCoerce (GEQ :: GOrdering () ())
        GT -> GGT

newTag :: PrimMonad m => m (Tag (PrimState m) a)
newTag = do
    u <- getUniq
    return (Tag u)

unsafeMkTag :: Word64 -> Tag s a
unsafeMkTag = Tag . unsafeMkUniq