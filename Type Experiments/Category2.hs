{-
 -      ``Category2''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        TypeOperators,
        MultiParamTypeClasses,
        FlexibleInstances,
        FlexibleContexts,
        TypeSynonymInstances,
        UndecidableInstances,
        FunctionalDependencies,
        ScopedTypeVariables
  #-}

module Category2 where

import qualified Prelude as P
import Prelude (Maybe(..), Monad(..))

-- categories (hom-function as binary type constructor)
class Category (~>) where
        id :: a ~> a
        (.) :: (b ~> c) -> (a ~> b) -> (a ~> c)

instance Category (->) where
        id = P.id
        (.) = (P..)

-- opposite morphisms & categories
newtype Flip t a b = Flip { unFlip :: t b a }
type (:<-) = Flip (->)

instance Category (~>) => Category (Flip (~>)) where
        id = Flip id
        (Flip f) . (Flip g) = Flip (g . f)

class (Category (~>), Category (~~>)) => Functor f (~>) (~~>) 
        -- I don't like this fundep, but it seem necessary
        -- for the Compose instance
        | f (~>) -> (~~>)
        where
        fmap :: (a ~> b) -> (f a ~~> f b)

-- functors
instance Functor ((,) a) (->) (->) where
        fmap f (a, b) = (a, f b)

-- instances from Prelude
instance Functor [] (->) (->) where
        fmap = P.fmap

instance Functor Maybe (->) (->) where
        fmap = P.fmap

instance Functor f (~>) (~~>) => ContraFunctor f (~>) (Flip (~~>)) where
        cmap f = Flip (fmap f)
instance Functor f (~>) (~~>) => ContraFunctor f (Flip (~>)) (~~>) where
        cmap (Flip f) = fmap f

instance Category (~>) => Functor ((~>) x) (~>) (->) where
        fmap = (.)

-- contravariant functors
class (Category (~>), Category (~~>)) => ContraFunctor f (~>) (~~>) where
        cmap :: (a ~> b) -> (f b ~~> f a)

instance Category (~>) => ContraFunctor (Flip (~>) x) (~>) (->) where
        cmap f g = Flip f . g
instance Category (~>) => ContraFunctor ((~>) x) (Flip (~>)) (->) where
        cmap (Flip f) g = f . g

-- adjoint functors

class (Functor f (~>) (~~>), Functor g (~~>) (~>))
        => Adjoint f g (~>) (~~>)
        | f g (~>) -> (~~>), f g (~~>) -> (~>) where
                uncurry :: (x ~> g y) -> (f x ~~> y)
                curry   :: (f x ~~> y) -> (x ~> g y)

instance Adjoint ((,) a) ((->) a) (->) (->) where
        curry f = \a x -> f (x, a)
        uncurry f = \(x,a) -> f a x

-- functor composition (a Haskell housekeeping thing; can't declare instances for )
newtype Compose f g x = RawCompose {rawUnCompose :: g (f x)}

class ComposeCat (~>) where
        compose   :: g (f x) ~> Compose f g x
        unCompose :: Compose f g x ~> g (f x)

instance ComposeCat (->) where
        compose = RawCompose
        unCompose = rawUnCompose

instance (Functor f (~>) (~~>), Functor g (~~>) (~~~>), ComposeCat (~~~>))
        => Functor (Compose f g) (~>) (~~~>)
        where fmap f = compose . fmap (fmap f) . unCompose

-- monads from adjoint functors (with major Compose wrapper gymnastics)
instance Adjoint f g (->) (->) => Monad (Compose f g) where
        return = compose . curry id
        x >>= f = join (fmap f x)
                where join = compose
                                . fmap (uncurry id)
                                . unCompose
                                . fmap (unCompose :: Compose f g b -> g (f b)) 

type State s a = Compose ((,) s) ((->)s) a

runState :: State s a -> s -> (s,a)
runState = unCompose
