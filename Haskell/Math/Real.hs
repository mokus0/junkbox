{-# LANGUAGE 
        MultiParamTypeClasses, FunctionalDependencies,
        FlexibleInstances, TypeSynonymInstances,
        UndecidableInstances, IncoherentInstances,
        NoImplicitPrelude, 
        EmptyDataDecls
  #-}
module Math.Real where

import Prelude hiding (Eq(..), Ord(..), (||), (&&), Num(..), Real(..))
import qualified Prelude as P
import Data.Unamb

-- a representation of the sierpinski space as lazy boolean values.
-- Computations which are known to possibly not terminate will return
-- this type rather than Bool, purely as a means of reminding the user 
-- of the (possibly unexpected) possibility of non-termination.
newtype Sigma = Sigma Bool
    deriving (Show)

terminates :: Sigma -> Bool
terminates (Sigma x) = x `seq` True

observe :: Sigma -> Bool
observe (Sigma x) = x `seq` x

Sigma x || Sigma y = Sigma ((x P.|| y) `unamb` (y P.|| x))

data Asc
data Desc

-- directed reals (nonempty streams of improving bounds)
-- The direction tag indicates direction of improvement (not direction of bounding).
-- So DReal Asc is an ascending sequence of lower bounds, for example.
data DReal dir = DReal !Rational !(Maybe (DReal dir))

fromList [x] = DReal x Nothing
fromList (x:xs) = DReal x (Just (fromList xs))

data Real = Real 
    { lBound :: !(DReal Asc)
    , uBound :: !(DReal Desc)
    }

void :: t
void = void

infinity :: DReal Asc
infinity = fromList (iterate (^2) 10)

negInfinity :: DReal Desc
negInfinity = negate infinity

class Eq a b where
    (==) :: a -> b -> Sigma

class Ord a b where
    (<) :: a -> b -> Sigma
    a < b = b > a
    
    (>) :: b -> a -> Sigma
    a > b = b < a

class Neq a b where
    (/=) :: a -> b -> Sigma

instance Ord Sigma Sigma where
    Sigma x > Sigma y = Sigma (x P.> y)

instance Ord (DReal Desc) Rational where
    DReal x Nothing   < y
        = x < y
    DReal x (Just xs) < y
        = (x < y) || (xs < y)
    
instance Ord Rational (DReal Asc) where
    DReal x Nothing   > y
        = x > y
    DReal x (Just xs) > y
        = (x > y) || (xs > y)
    
instance Ord (DReal Desc) (DReal Asc) where
    DReal x xs < DReal y ys
        =  (x < y) || (xs << ys)
        where
            Nothing << Nothing = Sigma False
            Nothing << Just y  = x < y
            Just x  << Nothing = x < y
            Just x  << Just y  = x < y

instance Ord Rational Rational where
    x < y = Sigma (x P.< y)

class FromInteger t where
    fromInteger :: Integer -> t

instance P.Num t => FromInteger t where
    fromInteger = P.fromInteger

instance FromInteger (DReal dir) where
    fromInteger i = x
        where x = DReal (P.fromInteger i) Nothing

instance Ord Real Real where
    Real dx ux < Real dy uy = ux < dy
    Real dx ux > Real dy uy = dx > uy

instance FromInteger Real where
    fromInteger i = Real (fromInteger i) (fromInteger i)

class Negate a b | a -> b, b -> a where
    negate :: a -> b

instance Negate (DReal Asc) (DReal Desc) where
    negate (DReal x xs) = DReal (P.negate x) (fmap negate xs)
instance Negate (DReal Desc) (DReal Asc) where
    negate (DReal x xs) = DReal (P.negate x) (fmap negate xs)

instance Negate Real Real where
    negate (Real d u) = Real (negate u) (negate d)
