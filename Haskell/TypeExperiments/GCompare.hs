{-# LANGUAGE GADTs #-}
module TypeExperiments.GCompare
    ( GOrdering(..)
    , GCompare(..)
    , geq
    , Tag
    , newTag
    , unsafeMkTag
    ) where

import TypeExperiments.Uniq
import Unsafe.Coerce
import Control.Monad.Primitive
import Control.Monad

-- |A type for the result of comparing GADT constructors; the type parameters
-- of the GADT values being compared are included so that in the case where 
-- they are equal their parameter types can be unified.
data GOrdering a b where
    GLT :: GOrdering a b
    GEQ :: GOrdering t t
    GGT :: GOrdering a b

-- |Type class for comparable GADT-like structures.  When 2 things are equal,
-- must return a witness that their parameter types are equal as well (GEQ).
class GCompare f where
    gcompare :: f a -> f b -> GOrdering a b

-- |Convenient function for simply obtaining a witness of type-equality, if one exists.
-- A handy idiom for using this would be to pattern-bind in the Maybe monad, eg.:
-- 
-- > extract :: Tag s a -> DSum (Tag s) -> Maybe a
-- > extract t1 (DSum t2 x) = do
-- >     GEQ <- geq t1 t2
-- >     return x
-- 
-- (Making use of the DSum type from TypeExperiments.Dependent, which is a 
-- standard dependent sum)
geq :: (GCompare f, MonadPlus m) => f a -> f b -> m (GOrdering a b)
geq a b = case gcompare a b of
    GEQ -> return GEQ
    _   -> mzero

-- |A super-special ad-hoc GADT-like thing. 'Tag's can be generated in any 
-- primitive monad (but only tags from the same one can be compared).  Every
-- tag is equal to itself and to no other.  The GOrdering class allows 
-- rediscovery of a tag's phantom type, useful for example with the "Env"
-- module in this same dir.
--
-- Essentially, a 'Tag' uses a 'Uniq' as a witness of type equality, which is
-- sound as long as the 'Uniq' is truly unique and only one 'Tag' is ever 
-- constructed from any given 'Uniq'.  'newTag' enforces these conditions.
-- 'unsafeMkTag' provides a way for adventurous users to take responsibility
-- for them.
data Tag s a where
    Tag :: Uniq s -> Tag s a
instance Show (Tag s a) where showsPrec p (Tag u) = showsPrec p u
instance GCompare (Tag s) where
    gcompare (Tag a) (Tag b) = case compare a b of
        LT -> GLT
        EQ -> unsafeCoerce (GEQ :: GOrdering () ())
        GT -> GGT

-- |Create a new tag witnessing a type @a@.  The tag can later be used to
-- recover the type @a@ through unification if @a@ was lost through 
-- existential quantification.
-- 
-- (I'm not sure whether the recovery is sound if @a@ is instantiated as a
-- polymorphic type, so I'd advise caution if you intend to try it.  I suspect 
-- it is, but I have not thought through it very deeply and certainly have not
-- proved it.)
newTag :: PrimMonad m => m (Tag (PrimState m) a)
newTag = do
    u <- getUniq
    return (Tag u)

-- |Very dangerous! This is essentially a deferred 'unsafeCoerce': by creating
-- a tag with this function, the user accepts responsibility for:
-- 
--  * Ensuring uniqueness of the Integer across the lifetime of the 'Tag'
--   (including properly controlling the lifetime of the 'Tag' by
--    universal quantification when discharging the 's' phantom type)
-- 
--  * Equivalently, ensuring that the phantom type 'a' is fixed (monomorphic) 
--    at the time the 'Tag' is created, so that the 'GCompare' instance is sound.
unsafeMkTag :: Integer -> Tag s a
unsafeMkTag = Tag . unsafeMkUniq