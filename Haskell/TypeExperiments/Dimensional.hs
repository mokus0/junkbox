{-# LANGUAGE
        GADTs, MultiParamTypeClasses, NoImplicitPrelude,
        KindSignatures, TypeFamilies, FlexibleContexts, 
        EmptyDataDecls, FunctionalDependencies, TypeSynonymInstances
  #-}
module Dimensional where

import Prelude hiding (Num(..), Integral(..), Floating(..))
import qualified Prelude as P

class Num a where
    fromInteger :: Integer -> a

-- for numeric literals
instance Num Integer    where fromInteger = P.fromInteger
instance Num Int        where fromInteger = P.fromInteger
instance Num Float      where fromInteger = P.fromInteger
instance Num Double     where fromInteger = P.fromInteger
instance Num Rational   where fromInteger = P.fromInteger

class Dim kind where
    type ReferenceUnit kind :: * -> *
    mkReferenceUnit   :: t -> ReferenceUnit kind t
    unmkReferenceUnit :: ReferenceUnit kind t -> t

class (Dim (UnitKind unit)) => Unit unit where
    type UnitKind unit
    toReferenceUnit :: (Num t, P.Floating t) =>  unit t -> t
    fromReferenceUnit :: (Num t, P.Floating t) => t -> unit t

data Time
newtype Second t = Second t deriving (Eq, Show)

instance Num t => Num (Second t) where 
    fromInteger i = Second (fromInteger i)

instance Dim Time where
    type ReferenceUnit Time = Second
    mkReferenceUnit = Second
    unmkReferenceUnit (Second x) = x

instance Unit Second where
    type UnitKind Second = Time
    toReferenceUnit (Second x) = x
    fromReferenceUnit = Second

data Length
newtype Meter t = Meter t deriving (Eq, Show)

instance Num t => Num (Meter t) where 
    fromInteger i = Meter (fromInteger i)

instance Dim Length where
    type ReferenceUnit Length = Meter
    mkReferenceUnit = Meter
    unmkReferenceUnit (Meter x) = x

instance Unit Meter where
    type UnitKind Meter = Length
    toReferenceUnit (Meter x) = x
    fromReferenceUnit = Meter

data Entropy
newtype Bit t = Bit t deriving (Eq, Show)

instance Num t => Num (Bit t) where 
    fromInteger i = Bit (fromInteger i)

instance Dim Entropy where
    type ReferenceUnit Entropy = Bit
    mkReferenceUnit = Bit
    unmkReferenceUnit (Bit x) = x

instance Unit Bit where
    type UnitKind Bit = Entropy
    toReferenceUnit (Bit x) = x
    fromReferenceUnit = Bit

newtype Nat t = Nat t deriving (Eq, Show)
instance Unit Nat where
    type UnitKind Nat = Entropy
    toReferenceUnit (Nat x) = x P.* P.log 2
    fromReferenceUnit x = Nat (x P./ P.log 2)

instance Num t => Num (Nat t) where
    fromInteger i = Nat (fromInteger i)

newtype Byte t = Byte t deriving (Eq, Show)
instance Unit Byte where
    type UnitKind Byte = Entropy
    toReferenceUnit (Byte x) = x P.* 8
    fromReferenceUnit x = Byte (x P./ 8)

instance Num t => Num (Byte t) where
    fromInteger i = Byte (fromInteger i)

newtype Micro unit t = Micro (unit t) deriving (Eq, Show)

instance Unit t => Unit (Micro t) where
    type UnitKind (Micro t) = UnitKind t
    toReferenceUnit (Micro t) = toReferenceUnit t P./ 1000000
    fromReferenceUnit x = Micro (fromReferenceUnit (1000000 P.* x))

instance Num (u t) => Num (Micro u t) where
    fromInteger i = Micro (fromInteger i)

newtype Milli unit t = Milli (unit t) deriving (Eq, Show)

instance Unit t => Unit (Milli t) where
    type UnitKind (Milli t) = UnitKind t
    toReferenceUnit (Milli t) = toReferenceUnit t P./ 1000
    fromReferenceUnit x = Milli (fromReferenceUnit (1000 P.* x))

instance Num (u t) => Num (Milli u t) where
    fromInteger i = Milli (fromInteger i)

newtype Kilo unit t = Kilo (unit t) deriving (Eq, Show)

instance Unit t => Unit (Kilo t) where
    type UnitKind (Kilo t) = UnitKind t
    toReferenceUnit (Kilo t) = toReferenceUnit t P.* 1000
    fromReferenceUnit x = Kilo (fromReferenceUnit (x P./ 1000))

instance Num (u t) => Num (Kilo u t) where
    fromInteger i = Kilo (fromInteger i)

infixl 6 +
(+) :: (Unit u1, Unit u2, Unit u3, 
        UnitKind u1 ~ UnitKind u2, 
        UnitKind u1 ~ UnitKind u3,
        UnitKind u2 ~ UnitKind u3,
        Num t, P.Floating t) => u1 t -> u2 t -> u3 t
x + y = fromReferenceUnit (toReferenceUnit x P.+ toReferenceUnit y)

convert :: (Unit u1, Unit u2, UnitKind u1 ~ UnitKind u2, Num t, P.Floating t)
    => u1 t -> u2 t
convert = fromReferenceUnit . toReferenceUnit