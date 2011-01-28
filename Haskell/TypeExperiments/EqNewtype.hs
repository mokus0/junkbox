{-# LANGUAGE TypeOperators, RankNTypes, KindSignatures #-}
module TypeExperiments.EqNewtype where

import Data.Eq.Type
import Unsafe.Coerce

newtype Foo a = Foo a
    deriving (Eq, Show)

-- This is passible, and "safe", but does not do what is expected, especially
-- when it comes to selecting instances.
foo :: a := Foo a
foo = Refl unsafeCoerce

unFoo :: Foo a := a
unFoo = symm foo

-- |@funky show@ could be used to 'show' any @[a]@, but would use the
-- @Show (Foo a)@ instance instead of the @Show a@ instance.
funky :: (f (Foo a) -> b) -> (f a -> b)
funky f = f . coerce (lift foo)

-- The := type would be a great place to have kind-polymorphism:
-- data (:=) (a :: k) (b :: k) = Refl { subst :: forall (c::k -> *). c a -> c b }

-- The particular case where k = * -> *:
data (::=) f g = Refl1 { subst1 :: forall (c :: (* -> *) -> *) x. c f -> c g }

refl1 :: (f ::= f)
refl1 = Refl1 id

trans1 :: (f ::= g) -> (g ::= h) -> (f ::= h)
trans1 (Refl1 fg) (Refl1 gh) = Refl1 (gh . fg)

newtype Symm1 p (c :: (* -> *) -> *) f g = Symm1 { unSymm1 :: p (c g) (c f)}
symm1 :: (f ::= g) -> (g ::= f)
symm1 fg = Refl1 (unSymm1 (subst1 fg (Symm1 id)))

newtype Lift1 (c :: (* -> *) -> (* -> *)) f g = Lift1 { unLift1 :: c f ::= c g}
lift1 :: (f ::= g) -> c f ::= c g
lift1 fg = unLift1 (subst1 fg (Lift1 refl1))

