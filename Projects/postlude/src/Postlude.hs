{-# LANGUAGE
        NoImplicitPrelude,
        MultiParamTypeClasses, FlexibleInstances,
        FlexibleContexts
        
  #-}
module Postlude where

import qualified Prelude as P
import Prelude 
    ( Functor(..)
    , Maybe(..), Either(..)
    , ($)
    , curry, uncurry
    
    , Eq(..), Bool(..)
    , Ord(..), Ordering(..)
    
    , Int, Integer
    , Rational, Float, Double
    )

import Data.Complex

-- Doesn't work... seems like it should...
-- class Functor f a where
--     map :: Functor f b => (a -> b) -> f a -> f b
-- instance P.Ord a => Functor S.Set a where
--     map = S.map

class Functor f => Pointed f where
    return :: a -> f a

class Pointed f => Applicative f where
    tuple :: f a -> f b -> f (a,b)
    ap :: f (a -> b) -> f a -> f b
    ap x y = fmap (uncurry ($)) (tuple x y)

class Applicative f => Monad f where
    (>>=) :: f a -> (a -> f b) -> f b
    join :: f (f a) -> f a

class Functor f => Copointed f where
    extract :: f a -> a

class Copointed f => Coapplicative f where
    cotuple :: f (Either a b) -> Either (f a) (f b)
    coAp :: f (Either (a -> c) (b -> c)) -> a -> b -> f c
    coAp x a b = case cotuple x of
        Left  f -> fmap ($a) f
        Right f -> fmap ($b) f

class Coapplicative f => Comonad f where
    extend :: f a -> (f a -> b) -> f b

class Num a where
    fromInteger :: Integer -> a

class Frac a where
    fromRational :: Rational -> a

class Add a where
    zero :: a
    isZero :: Eq a => a -> Bool
    (+) :: a -> a -> a

class Multiply a where
    one :: a
    isOne :: Eq a => a -> Bool
    (*) :: a -> a -> a

class Negate a where
    negate :: a -> a
    (-) :: Add a => a -> a -> a
    subtract :: Add a => a -> a -> a

class Divide a where
    recip :: a -> a
    (/) :: Multiply a => a -> a -> a

