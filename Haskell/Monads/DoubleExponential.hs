module Monads.DoubleExponential where

import Control.Comonad
import Control.Monad.Instances

-- the exponential bifunctor;
-- contravariant argument placed last, because that's the one
-- we're interested in here.
newtype E x y = E { runE :: y -> x }

-- 'E x' is a contravariant functor.
-- its action on arrows is postcomposition.
cmap :: (a -> b) -> E x b -> E x a
cmap f (E g) = E (g . f)

-- That functor is self-adjoint, and the hom-isomorphism is 'flip' (modulo newtype wrapping)
adj :: (a -> E x b) -> (b -> E x a)
adj f = E . flip (runE . f)

-- the unit/counit:
eta :: a -> E x (E x a)
eta = adj id

-- the zig/zag transformations
zig :: E x (E y (E y a)) -> E x a
zig = cmap eta

zag :: E y a -> E x (E x (E y a))
zag = eta

-- mu = zig 
-- (whiskered by @E@ on the right, which corresponds to simply refining the type)
mu :: E x (E y (E y (E z a))) -> E x (E z a)
mu = zig

-- the zig-zag equation (@zag . zig = id@ is the same equation because the 
-- composition happens in the opposite category):
--      zig . zag = id
-- it's not actually necessary to prove this in Haskell because parametricity
-- ensures naturality of adj, but just for fun here we go:
    --      zig . zag
    --    = cmap eta . eta
    --    = cmap (adj id) . adj id
    --    = cmap (adj id) . E . flip runE
    --    = \x -> cmap (adj id) (E (flip runE x))
    --    = \x -> E (flip runE x . adj id)
    --    = \x -> E (flip runE x . E . flip runE)
    --    = \x -> E ((\y -> flip runE x (E y)) . flip runE)
    --    = \x -> E ((\y -> runE (E y) x) . flip runE)
    --    = \x -> E ((\y -> y x) . flip runE)
    --    = \x -> E (\z -> ($ x) (flip runE z))
    --    = \x -> E (\z -> flip runE z x)
    --    = \x -> E (\z -> runE x z)
    --    = \x -> E (runE x)
    --    = \x -> x
    --    = id

-- since @E x@ is self-adjoint, composing it with itself gives a monad
-- (or dually, a comonad in the opposite category; they are effectively the same)
newtype K x y = K { runK :: E x (E x y) }

instance Functor (K x) where
    fmap f = K . cmap (cmap f) . runK
instance Monad (K x) where
    return = K . eta
    x >>= f = joinK (fmap f x)

joinK :: K x (K x y) -> K x y
joinK = K . mu . runK . fmap runK

-- runCont = ($)
-- (evaluate a double-exponential at the object corresponding to the upper exponential)
runCont :: K x y -> E x y -> x
runCont = runE . runK

-- this is to be thought of as a global element of the exponential object.
-- Abstraction (a meta-operation of type @((g,a) -> b) -> (g -> E b a)@) constructs
-- the universal morphism of the exponential diagram; abstracting the identity
-- morphism (from 1 * x -> 1 * x) gives the identity element of the exponential
-- @E x x@.
i :: E x x
i = E id

-- Since every exponential of the form @E x x@ has a global element, evaluation at that element gives a
-- morphism (K x x -> x).
runCont_ :: K x x -> x
runCont_ x = runCont x i
