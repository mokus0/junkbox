{-# LANGUAGE NoImplicitPrelude #-}
module Num where

import qualified Prelude as P
import Prelude (Int, Integer, Float, Double)

class IntLiteral t where
    fromInteger :: Integer -> t

class IntLiteral t => FracLiteral t where
    fromRational :: P.Rational -> t

class IsZero t where
    isZero :: t -> Bool

class IsOne t where
    isOne :: t -> Bool

class Additive t where
    zero :: t
    (+) :: t -> t -> t

class Additive t => AdditiveGroup t where
    negate      :: t -> t
    (-)         :: t -> t -> t
    subtract    :: t -> t -> t

class Multiplicative t where
    one :: t
    (*) :: t -> t -> t

class (AdditiveGroup t, Multiplicative t) => Ring t

class Ring t => Field t where
    (/) :: t -> t -> t
    recip :: t -> t

class Ring t => Integral t where
    div    :: t -> t -> t
    mod    :: t -> t -> t
    divMod :: t -> t -> (t,t)

class (IsZero t, IsOne t, Ord t) => Real t where
    abs         :: t -> t
    signum      :: t -> t
    toRational  :: t -> Rational

class Module a b where
    scale :: a -> b -> b

class (Ring r, Additive v, Module r v) => VectorSpace r v
