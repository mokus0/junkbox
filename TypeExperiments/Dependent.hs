{-# LANGUAGE 
        GADTs, 
        ExistentialQuantification, 
        KindSignatures, 
        ScopedTypeVariables,
        ImpredicativeTypes,
        RankNTypes
  #-}
module TypeExperiments.Dependent where

data DSum tag = forall a. DSum (tag a) a
(>!) :: DSum tag -> (forall a. tag a -> a -> b) -> b
(DSum tag x) >! f = f tag x

data DProd tag = DProd (forall a. tag a -> a)
(!) :: DProd tag -> tag a -> a
(DProd f) ! tag = f tag

in_ con x = DSum con x
out con p = p ! con

data E a b c where
    L :: E a b a
    R :: E a b b

type Either' a b = DSum  (E a b)
type Pair    a b = DProd (E a b)

either' :: (a -> x) -> (b -> x) -> Either' a b -> x
either' f g (DSum L x) = f x
either' f g (DSum R x) = g x

pair :: (x -> a) -> (x -> b) -> x -> Pair a b
pair f g x = DProd (p f g x)
    where
        p :: (x -> a) -> (x -> b) -> x -> E a b c -> c
        p f g x L = f x
        p f g x R = g x

data M :: * -> * -> * where
    J :: M a a
    N :: M a ()

type Maybe' a = DSum (M a)

maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' z f (DSum J x ) = f x
maybe' z f (DSum N ()) = z

-- type Foo a = DProd (M a)
-- foo x = DProd (f x)
--     where
--         f :: a -> M a b -> b
--         f x J = x
--         f x N = ()

data Foo b tag x where
    Foo :: tag a -> Foo b tag (a -> b)
