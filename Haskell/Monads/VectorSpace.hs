{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Monads.VectorSpace where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Writer
import Data.Complex
import qualified Data.Foldable as F
import Data.Function
import qualified Data.Map as M
import Data.Monoid
import Data.VectorSpace
import Unsafe.Coerce
import qualified Data.Vector as V

-- the free vector space over s with coefficients in k
newtype V k s = V (WriterT (Product k) V.Vector s)
    deriving (Functor, Applicative, Monad, MonadPlus)

e :: Num k => s -> V k s
e = return

v :: V.Vector (a,b) -> V b a
v = unsafeCoerce

unV :: V b a -> V.Vector (a,b)
unV = unsafeCoerce

mapV :: (b -> c) -> V b a -> V c a
mapV f (V x) = V (mapWriterT (V.map (fmap f')) x)
    where f' (Product p) = Product (f p)

real s = v (V.singleton ((), s))

l1 :: Num k => V k s -> k
l1 = V.sum . V.map snd . unV

instance (Num k, s ~ ()) => Num (V k s) where
    fromInteger s = real (fromInteger s)
    (+) = mplus
    (*) = (>>)
    negate = (*) (real (-1))
    abs    = real . abs    . l1
    signum = real . signum . l1

instance (Real k, s ~ ()) => Real (V k s) where
    toRational = toRational . l1

instance (Num k, Enum k, s ~ ()) => Enum (V k s) where
    toEnum = real . toEnum
    fromEnum = fromEnum . l1

instance (Integral k, AdditiveGroup k, s ~ ()) => Integral (V k s) where
    toInteger x = toInteger (x <.> 1)
    quotRem p q = (real d, real m)
        where (d,m) = quotRem (l1 p) (l1 q)
    divMod p q = (real d, real m)
        where (d,m) = divMod (l1 p) (l1 q)


collect :: (Num k, Ord s) => V.Vector (s, k) -> M.Map s k
collect = M.fromListWith (+) . V.toList

reduce' :: (Num k, Eq k, Ord s) => V.Vector (s,k) -> V.Vector (s,k)
reduce' = V.fromList . M.toList . M.filter (0 /=) . collect

reduce :: (Num k, Eq k, Ord s) => V k s -> V k s
reduce = v . reduce' . unV

instance (Num k, Eq k, Ord s) => Eq (V k s) where
    (==) = (==) `on` reduce' . unV

instance (Num k, Ord k, Ord s) => Ord (V k s) where
    compare = compare `on` reduce' . unV

instance (Show k, Show a) => Show (V k a) where
    showsPrec p xs = showParen (p > 10)
        ( showString "v "
        . showsPrec 11 (unV xs)
        )

instance Num k => AdditiveGroup (V k s) where
    zeroV = mzero
    (^+^) = mplus
    negateV x = (-1) >> x

instance Num k => VectorSpace (V k s) where
    type Scalar (V k s) = k
    s *^ x = real s >> x

instance (Num k, AdditiveGroup k, Ord s) => InnerSpace (V k s) where
    x <.> y = F.sum (M.intersectionWith (*) (collect (unV x)) (collect (unV y)))

-- Bayes' rule is "just" normalization relative to the L1 norm
-- (after discarding outcomes known by prior knowledge to be impossible / inapplicable)
bayes :: Fractional b => V b a -> V b a
bayes v = v ^/ l1 v 

-- for example:
data Test = Pos | Neg
    deriving (Eq, Ord, Show)

data Truth = User | Clean
    deriving (Eq, Ord, Show)

percent :: Fractional b => b -> a -> a -> V b a
percent p t f = s *^ e t ^+^ (1-s) *^ e f
    where s = p / 100

drugTest1 = do
    truth <- percent 0.1 User Clean
    test <- case truth of
        User    -> percent 99 Pos Neg
        Clean   -> percent  1 Pos Neg
    
    return (truth, test)

drugTest2 = bayes $ do
    (truth, test) <- drugTest1
    guard (test == Pos)
    return truth

type Q a = V (Complex a)

hadamard :: Bool -> Q Double Bool
hadamard x =
    if x
        then v (V.fromList [(True, sqrt 0.5), (False, -sqrt 0.5)])
        else v (V.fromList [(True, sqrt 0.5), (False,  sqrt 0.5)])

qNot :: Bool -> Q Double Bool
qNot = hadamard >=> hadamard

measure :: (a ~ Scalar a, RealFloat a, InnerSpace a) => Q a t -> V a t
measure = mapV magnitudeSq

l2 :: (Floating a) => Q a t -> a
l2 = sqrt . V.sum . V.map (magSq . snd) . unV
    where magSq (a :+ b) = a*a + b*b


-- partial measurement (at probability p) corresponds to evolution
-- by the following operator (a controlled rotation), followed by 
-- observation of only the second qbit.
--
-- After this operation, if the first qbit is True then the second has
-- probability 'p' of being True; if the first qbit is False then the
-- second qbit is False with probability 1 (?)
u :: Double -> Bool -> Bool -> Q Double (Bool, Bool)
u p True  True = (sqrt (1 - p) :+ 0) *^ e (True, True)  ^+^ (sqrt p :+ 0) *^ e (True, False)
u p False True = (sqrt (1 - p) :+ 0) *^ e (True, False) ^-^ (sqrt p :+ 0) *^ e (True, True)
u _ s False    = return (False, s)

observe :: (RealFloat a, InnerSpace a, a ~ Scalar a, Ord b, Ord c) => Q a (b, c) -> V a (b, Q a c)
observe st = measure $ do
    (m, _) <- st
    let st' = normalized $ do
            (x, y) <- st
            guard (x == m)
            return y
    return (m, st')

pMeasure p x = observe (x >>= u p True)
