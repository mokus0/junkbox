{-
 -      ``Stack''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    TypeSynonymInstances,
    FlexibleInstances,
    NoImplicitPrelude
  #-}
module Stack where

import Prelude hiding ((+), (-), (*), (/), drop, (>>), fromInteger)
import qualified Prelude as P

type Push s a = s -> (a,s)

instance Show a => Show (Push s a) where
    showsPrec p f = case f undefined of
        (x, _) -> showsPrec p x

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

fix f = f (fix f)

run f = fst (f ())
-- 'run' with a stack that can be negative!
run2 f = fst (fix (drop >> f))

dup (a,s) = (a,(a,s))
drop (a,s) = s
roll (a,(b,(c,s))) = (b,(c,(a,s)))

infixl 9 >>
(>>) = flip (.)

fromInteger :: Integer -> s -> (Integer, s)
fromInteger = (,)

-- fromInteger :: Integer -> ((Integer, a) -> c) -> a -> c
-- fromInteger n = (fromInteger' n >>)