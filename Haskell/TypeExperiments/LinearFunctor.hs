module TypeExperiments.LinearFunctor where

import Control.Monad (liftM2)

-- |Functors where f(a + b) = f(a) + f(b)
class Functor f => Linear f where
    distrib :: f (Either a b) -> Either (f a) (f b)

undistrib :: Functor f => Either (f a) (f b) -> f (Either a b)
undistrib = either (fmap Left) (fmap Right)

-- |Functors where f(a * b) = f(a) * f(b)
class Functor f => CoLinear f where
    codistrib ::  f a -> f b -> f (a,b)

uncodistrib :: Functor f => f (a,b) -> (f a, f b)
uncodistrib f = (fmap fst f, fmap snd f)

class Functor f => Pointed f where
    -- I'd say these default implementations are the "laws" a pointed functor ought to satisfy.  Essentially, they say that every "pure" value must have the same "shape".  Though, I think they pretty much do anyway - 'unit' could be a top-level definition and the laws would be automatic by parametricity, wouldn't they?
    unit :: f ()
    unit = pure ()
    pure :: a -> f a
    pure x = fmap (const x) unit

f <*> x = fmap (uncurry ($)) (codistrib f x)

monadPure :: Monad m => a -> m a
monadPure = return
monadCodistrib :: Monad m => m a -> m b -> m (a,b)
monadCodistrib = liftM2 (,)

