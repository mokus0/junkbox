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

-- the free vector space over s with coefficients in k
newtype V k s = V (WriterT (Product k) [] s)
    deriving (Functor, Applicative, Monad, MonadPlus)

e :: Num k => s -> V k s
e = return

v :: [(a,b)] -> V b a
v = unsafeCoerce

unV :: V b a -> [(a,b)]
unV = unsafeCoerce

real s = v [((), s)]

l1 :: Num k => V k s -> k
l1 = sum . map snd . unV

instance (Num k, s ~ ()) => Num (V k s) where
    fromInteger s = v [((), fromInteger s)]
    (+) = mplus
    (*) = (>>)
    negate = (*) (-1)
    abs    = real . abs    . l1
    signum = real . signum . l1

instance (Real k, s ~ ()) => Real (V k s) where
    toRational = toRational . l1

instance (Num k, Enum k, s ~ ()) => Enum (V k s) where
    toEnum = real . toEnum
    fromEnum = fromEnum . l1

instance (Integral k, s ~ ()) => Integral (V k s) where
    toInteger x = toInteger (x <.> 1)
    quotRem p q = (real d, real m)
        where (d,m) = quotRem (l1 p) (l1 q)
    divMod p q = (real d, real m)
        where (d,m) = divMod (l1 p) (l1 q)


collect :: (Num k, Ord s) => [(s, k)] -> M.Map s k
collect = M.fromListWith (+)

reduce' :: (Num k, Eq k, Ord s) => [(s,k)] -> [(s,k)]
reduce' = M.toList . M.filter (0 /=) . collect

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

instance (Num k, Ord s) => InnerSpace (V k s) where
    x <.> y = F.sum (M.intersectionWith (*) (collect (unV x)) (collect (unV y)))

-- Bayes' rule is "just" normalization relative to the L1 norm
-- (after discarding outcomes known by prior knowledge to be impossible / inapplicable)
bayes v = v ^/ l1 v 

-- for example:
data Test = Pos | Neg
    deriving (Eq, Ord, Show)

data Truth = User | Clean
    deriving (Eq, Ord, Show)

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

