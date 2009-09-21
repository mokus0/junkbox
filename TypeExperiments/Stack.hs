{-
 -      ``Stack''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    TypeSynonymInstances,
    FlexibleInstances,
    NoImplicitPrelude, IncoherentInstances
  #-}
module TypeExperiments.Stack where

import Prelude hiding ((+), (-), (*), (/), drop, (>>), fromInteger, fromRational, sqrt, flip, pi)
import qualified Prelude as P

type Push s a = s -> (a,s)

instance Show a => Show (Push s a) where
    showsPrec p f = case f undefined of
        (x, _) -> showsPrec p x

instance Show a => Show (s -> a) where
    showsPrec p = (P.$ error "bottom of stack") >> showsPrec p

instance Eq a => Eq (Push s a) where
    x == y  = fst (x undefined) == fst (y undefined)

instance (Num a) =>  Num (Push s a) where
    fromInteger n = \s -> (P.fromInteger n, s)

liftS  f   (x, s) = (f x, s)
liftS2 (*) (x,(y,s)) = (x * y, s)

(+) = liftS2 (P.+)
(-) = liftS2 (P.-)
(*) = liftS2 (P.*)
(/) = liftS2 (P./)

sqrt = liftS P.sqrt

fix f = f (fix f)

run f = fst (f ())
-- 'run' with a stack that can be negative!
run2 f = fst (fix (drop >> f))

dup (a,s) = (a,(a,s))
drop (a,s) = s
flip (a,(b,s)) = (b,(a,s))
roll (a,(b,(c,s))) = (b,(c,(a,s)))

sqr = dup >> (*)
pythag = sqr >> flip >> sqr >> (+) >> sqrt

infixl 9 >>
(>>) = P.flip (P..)

push :: a -> Push s a
push = (,)

fromInteger :: Num a => Integer -> Push s a
fromInteger = push . P.fromInteger

fromRational :: Fractional a => Rational -> Push s a
fromRational = push . P.fromRational

pi :: Push s Double
pi = fromRational (toRational (P.pi :: Double))

-- fromInteger :: Integer -> ((Integer, a) -> c) -> a -> c
-- fromInteger n = (fromInteger' n >>)