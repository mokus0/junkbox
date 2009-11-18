{-
 -      ``Category2''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -
 -      This is probably the first time I have ever given serious
 -      thought to what it would be like to have polymorhic kinds.
 -      That would make things pretty nifty here.
 -}
{-# LANGUAGE
        TypeOperators,
        MultiParamTypeClasses,
        FlexibleInstances,
        FlexibleContexts,
        TypeSynonymInstances,
        UndecidableInstances,
        IncoherentInstances,
        FunctionalDependencies,
        ScopedTypeVariables,
        ExistentialQuantification,
        GADTs,
        TypeFamilies
  #-}

module TypeExperiments.Category2 where

import qualified Prelude as P
import Prelude (Eq(..), Show(..), Maybe(..), Either(..), Monad(..), undefined)

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
data Diag a = Pair a a
        deriving (Eq, Show)

instance Functor Diag (->) (->) where
        fmap f (Pair x y) = Pair (f x) (f y)

instance Functor ((,) a) (->) (->) where
        fmap f (a, b) = (a, f b)

-- instances from Prelude
instance Functor [] (->) (->) where
        fmap = P.fmap

instance Functor Maybe (->) (->) where
        fmap = P.fmap

instance Functor f (~>) (~~>) => ContraFunctor f (~>) (Flip (~~>)) where
        cmap f = Flip (fmap f)
-- Bah, I can't do this, because of that fundep I had to add:
-- instance ContraFunctor f (~>) (~~>) => Functor f (~>) (Flip (~~>)) where
--         fmap f = Flip (cmap f)
instance Functor f (~>) (~~>) => ContraFunctor f (Flip (~>)) (~~>) where
        cmap (Flip f) = fmap f
-- at least I can do this, anyway:
instance ContraFunctor f (~>) (~~>) => Functor f (Flip (~>)) (~~>) where
        fmap (Flip f) = cmap f

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

rAdj :: (Adjoint f g (~>) (~~>)) => (f x ~~> y) -> (x ~> g y)
rAdj = curry
lAdj :: (Adjoint f g (~>) (~~>)) => (x ~> g y) -> (f x ~~> y)
lAdj = uncurry

unit :: (Adjoint f g (~>) (~~>)) => x ~> g (f x)
unit = rAdj id
coUnit :: (Adjoint f g (~>) (~~>)) => f (g y) ~~> y
coUnit = lAdj id

-- functor composition (a Haskell housekeeping thing; can't declare instances for ad-hoc type functions)
newtype Compose f g x = RawCompose {rawUnCompose :: g (f x)}

class Category (~>) => ComposeCat (~>) where
        compose   :: g (f x) ~> Compose f g x
        unCompose :: Compose f g x ~> g (f x)

instance ComposeCat (->) where
        compose = RawCompose
        unCompose = rawUnCompose

instance (Functor f (~>) (~~>), Functor g (~~>) (~~~>), ComposeCat (~~~>))
        => Functor (Compose f g) (~>) (~~~>)
        where fmap f = compose . fmap (fmap f) . unCompose

-- ContraFunctor composition would be nice too...

-- monads from adjoint functors (with major Compose wrapper gymnastics)
instance Adjoint f g (->) (->) => Monad (Compose f g) where
        return = compose . curry id
        x >>= f = join (fmap f x)
                where join = compose
                                . fmap (uncurry id)
                                . unCompose
                                . fmap (unCompose :: Compose f g b -> g (f b)) 

-- comonads
class Functor c (->) (->) => Comonad c where
        coReturn :: c x -> x
        coJoin :: c x -> c (c x)
        coBind :: c b -> (c b -> a) -> c a -- (?)
        coBind x f = fmap f (coJoin x)

instance Adjoint f g (->) (->) => Comonad (Compose g f) where
        coReturn = uncurry id . unCompose
        coJoin = fmap (compose :: f (g b) -> Compose g f b)
                . compose
                . fmap (curry id)
                . unCompose

-- state monad, arising from adjunction between tuple and arrow
instance Adjoint ((,) a) ((->) a) (->) (->) where
        curry f = \a x -> f (x, a)
        uncurry f = \(x,a) -> f a x

type State s a = Compose ((,) s) ((->)s) a
type CoState s a = Compose ((->)s) ((,)s) a

runState :: State s a -> s -> (s,a)
runState = unCompose

runCoState :: CoState s a -> (s, s -> a)
runCoState = unCompose

-- Identity monad/comonad, arising from self-adjointness of Identity functor (ooh, fancy that!)
newtype IdentityFunctor a = IdentityFunctor a
instance IdentityFunctorCat (~>) => Functor IdentityFunctor (~>) (~>) where
        fmap f = identityFunctor . f . runIdentityFunctor

class Category (~>) => IdentityFunctorCat (~>) where
        identityFunctor :: a ~> IdentityFunctor a
        runIdentityFunctor :: IdentityFunctor a ~> a

instance IdentityFunctorCat (->) where
        identityFunctor = IdentityFunctor
        runIdentityFunctor (IdentityFunctor a) = a

instance IdentityFunctorCat (~>) => Adjoint IdentityFunctor IdentityFunctor (~>) (~>) where
        curry f = identityFunctor . f . identityFunctor
        uncurry g = runIdentityFunctor . g . runIdentityFunctor

type Identity a = Compose IdentityFunctor IdentityFunctor a
runIdentity :: (ComposeCat (~>), IdentityFunctorCat (~>)) => Identity a ~> a
runIdentity = runIdentityFunctor . runIdentityFunctor . unCompose

--   Subcategories (argh, this panics my ghc at home, in both of these forms:)
-- data SubCat a b (~>) c d = (a ~ c, b ~ d) => SubCat {unSubCat :: a ~> b}
-- data (a ~ c, b ~ d) => SubCat a b (~>) c d = SubCat {unSubCat :: a ~> b}
--   This one panics even more verbosely:
-- newtype (a ~ c, b ~ d) => SubCat a b (~>) c d = SubCat {unSubCat :: a ~> b}
--

-- it accepts this one, but then panics on the instance... grrr.
-- data SubCat a b (~>) c d where
--         SubCat :: (a ~> b) -> SubCat a b (~>) a b
-- although, the instance still won't give me what I want anyway.  What I really
-- want is a way to put a context on the "phantom" parameters and have
-- the category restricted to that context
-- instance Category (~>) => Category (SubCat a b (~>)) where
--         id = SubCat id
--         (SubCat f) . (SubCat g) = SubCat (f.g)

-- scratch space

-- argh, another panic here (triggerred by the pattern binding in the Category instance):
-- data Monoid (~>) a b where
--         MArr :: (t ~> t) -> Monoid (~>) t t

newtype Graph (~>) a b = Arr (a ~> b)
instance Category (~>) => Category (Graph (~>)) where
        id = Arr id
        (Arr f) . (Arr g) = Arr (f.g)

type Monoid (~>) t = Graph (~>) t t
mempty :: Category (~>) => Monoid (~>) t
mempty = id
mappend :: Category (~>) => Monoid (~>) t -> Monoid (~>) t -> Monoid (~>) t
mappend = (.)

-- now, how to actually construct a category such that I can say:
--   "hello " `mappend` "world!"
-- (and have it evaluate to the concatenation)

-- can the "fold" concept be expressed as / derived from an adjunction?
-- is it sensible to do so?

data Groupoid (~>) a b = GArr (a ~> b) (b ~> a)
instance Category (~>) => Category (Groupoid (~>)) where
        id = GArr id id
        GArr f f' . GArr g g' = GArr (f.g) (g'.f')

instance Functor (Either a) (->) (->) where
        fmap f (Left a) = Left a
        fmap f (Right b) = Right (f b)

instance Functor (Flip Either a) (->) (->) where
        fmap f (Flip (Left a)) = Flip (Left (f a))
        fmap f (Flip (Right b)) = Flip (Right b)

instance Functor (Flip (,) a) (->) (->) where
        fmap f (Flip (a,b)) = Flip (f a, b)

instance Adjoint (Flip (,) a) ((->) a) (->) (->) where
        curry f = \a x -> f (Flip (a,x))
        uncurry g = \(Flip (a,x)) -> g a x

data Zero a = Zero
data Const a x = Const a

-- can these really be said to have adjoints?  They seem to be
-- extremely degenerate cases.  It may be that they cannot satisfy the naturality part.
instance Category (~>) => Functor Zero (~>) (->) where
        fmap f = \Zero -> Zero

instance Category (~>) => Functor (Const a) (~>) (->) where
        fmap f = \(Const a) -> (Const a)

-- Diagonal functor left adjoint to product
-- (I've heard that this is true, but I can't quite resolve it in my mind it a meaningful formalism)
-- instance Adjoint Diag ((,) a) (->) (->) where
--         curry :: (Diag x -> y) -> (x -> (a,y))
--         uncurry :: (x -> (a,y)) -> (Diag x -> y)

-- is there a dual concept to 'currying'?
-- curry :: ((a,b) -> c) -> (a -> (b -> c))
-- coCurry :: (c -> Either a b) -> (c -> ??)

---- curry arises as an isomophism, from adjoint functors.  What exactly
---- would it mean to 'dualize' that?
---- coCurry ::

-- What adjunction gives rise to the [] monad in Prelude?
-- rAdj :: (f x -> y) -> (x -> g y)
-- lAdj :: (x -> g y) -> (f x -> y)
--   unit :: x -> [x]
--   unit = rAdj id
--   