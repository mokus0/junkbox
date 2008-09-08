{-
 -      ``SubType''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances,
        UndecidableInstances,
        OverlappingInstances,
        IncoherentInstances,
        TypeSynonymInstances
  #-}

module SubType where

import Complex
import Control.Monad

class SubType a b where
        promote :: a -> b

instance SubType a a where
        promote = id

instance Integral a => SubType a Integer where
        promote = toInteger

instance Real a => SubType a Rational where
        promote = toRational

instance SubType Float Double where
        promote = realToFrac

instance RealFloat a => SubType a (Complex a) where
        promote = (:+ 0)

instance MonadPlus m => SubType () (m a) where
        promote () = mzero

instance MonadPlus m => SubType (Maybe a) (m a) where
        promote = maybe mzero return

instance MonadPlus m => SubType [a] (m a) where
        promote = foldr (mplus.return) mzero

instance (Functor f, SubType a b) => SubType (f a) (f b) where
        promote = fmap promote

instance (SubType a1 a2, SubType b1 b2) => SubType (a1,b1) (a2,b2) where
        promote (a,b) = (promote a, promote b)

instance (SubType a1 a2, SubType b1 b2) => SubType (Either a1 b1) (Either a2 b2) where
        promote (Left a) = Left (promote a)
        promote (Right b) = Right (promote b)

instance SubType (Either a b) (Maybe a, Maybe b) where
        promote (Left a) = (Just a, Nothing)
        promote (Right b) = (Nothing, Just b)

instance SubType a b => SubType (b -> c) (a -> c) where
        promote f = f.promote

instance SubType a b => SubType (c -> a) (c -> b) where
        promote f = promote.f

instance (SubType a b, SubType c d) => SubType (b -> c) (a -> d) where
        promote f = promote.f.promote

-- questionable instances below here
-- though things like these are typeable, is it really accurate to say them?
-- it seems some stronger condition needs to be at work here.
instance Functor f => SubType (a -> b) (f a -> f b) where
        promote = fmap

instance Monad m => SubType a (m a) where
        promote = return

instance (Monad m) => SubType (m (m a)) (m a) where
        promote = join

instance Functor f => SubType (f (a,b)) (f a, f b) where
        promote thing = (fmap fst thing, fmap snd thing)

instance Functor f => SubType (Either (f a) (f b)) (f (Either a b)) where
        promote (Left xs) = fmap Left xs
        promote (Right ys) = fmap Right ys

instance SubType (a,[a]) [a] where
        promote = uncurry (:)

instance SubType ((a,b) -> c) (a -> b -> c) where
        promote = curry

instance SubType (a -> b -> c) ((a,b) -> c) where
        promote = uncurry

--   this one, while strictly true, is not something ghc can handle without
--   fundeps or phantom types and newtype wrappers, etc.
-- instance (SubType a b, SubType b c) => SubType a c where
--         promote = (promote :: b -> c) . (promote :: a -> b)

--   this one is just plain weird.  essentially it defines a top type, if its
--   context could possibly be satisfied (which it probably cannot be)
-- instance (SubType a b, SubType (a -> b) c) => SubType x c where
--         promote _ = promote promote

