{-# LANGUAGE
        MultiParamTypeClasses,
        FunctionalDependencies,
        FlexibleInstances,
        FlexibleContexts,
        UndecidableInstances,
        TypeSynonymInstances,
        TypeFamilies
  #-}
module Math.CayleyDickson where

import Data.VectorSpace

-- A strict tuple type with a suggestive name:
data CD a = CD !a !a
    deriving Eq

-- The first few steps of the construction have names:
type Complex    a = CD a
type Quaternion a = CD (Complex a)
type Octonion   a = CD (Quaternion a)
type Sedenion   a = CD (Octonion a)

-- and short-hand forms:
type C a = CD a
type H a = CD (C a)
type O a = CD (H a)
type S a = CD (O a)

instance Show a => Show (CD a) where
    showsPrec p (CD a b) = showParen (p == 0) -- ???
        ( showsPrec 1 a
        . showChar ','
        . showsPrec 1 b
        )

class Num a => Conj a where
    conj :: a -> a

instance Conj Int       where conj = id
instance Conj Integer   where conj = id
instance Conj Float     where conj = id
instance Conj Double    where conj = id
instance Conj Rational  where conj = id

instance Conj a => Num (CD a) where
    fromInteger x = CD (fromInteger x) 0
    CD a1 b1 + CD a2 b2 = CD (a1+a2) (b1+b2)
    CD a1 b1 - CD a2 b2 = CD (a1-a2) (b1-b2)
    negate (CD a b) = CD (negate a) (negate b)
    CD a b * CD c d = CD
        (a*c - conj d * b)
        (d*a + b * conj c)

instance Conj a => Conj (CD a) where
    conj (CD a b) = CD (conj a) (negate b)

instance VectorSpace Int where
    type Scalar Int = Int
    (*^) = (*)

instance InnerSpace Int where
    (<.>) = (*)

instance VectorSpace Integer where
    type Scalar Integer = Integer
    (*^) = (*)

instance InnerSpace Integer where
    (<.>) = (*)

instance InnerSpace Rational where
    (<.>) = (*)

instance Conj a => AdditiveGroup (CD a) where
    zeroV = 0
    negateV = negate
    (^+^) = (+)

instance (Conj a, VectorSpace a, AdditiveGroup (Scalar a)) => VectorSpace (CD a) where
    type Scalar (CD a) = Scalar a
    s *^ CD a b = CD (s *^ a) (s *^ b)

instance (Conj a, InnerSpace a, AdditiveGroup (Scalar a)) => InnerSpace (CD a) where
    CD a b <.> CD c d = (a <.> c) ^+^ (b <.> d)


class Conj a => Basis a where
    basis :: [a]

instance Basis Int       where basis = [1]
instance Basis Integer   where basis = [1]
instance Basis Float     where basis = [1]
instance Basis Double    where basis = [1]
instance Basis Rational  where basis = [1]

instance Basis a => Basis (CD a) where
    basis = [lift e | lift <- [\x -> CD x 0, \x -> CD 0 x], e <- basis]

e i = basis !! i

i :: Basis a => C a
i = e 1

j,k :: Basis a => H a
j = e 2
k = e 3

l,m,n,o :: Basis a => O a
l = e 4
m = e 5
n = e 6
o = e 7

p,q,r,s,t,u,v,w :: Basis a => S a
p = e 8
q = e 9
r = e 10
s = e 11
t = e 12
u = e 13
v = e 14
w = e 15

-- Note that for H and beyond, (/) is ambiguous:  
-- @p * recip q@ is NOT the same as @recip q * p@.
instance (Fractional a, Conj a) => Fractional (CD a) where
    fromRational a = CD (fromRational a) 0
    recip x@(CD a b) = CD (conj a / m) (negate b / m)
        where m = magSq x
    
    x / y = x * recip y

x \\ y = recip x * y
infixr 7 \\

magSq (CD a b) = a * conj a + conj b * b

realPart q = 0.5 * (q + conj q)
imagPart q = q - realPart q

expSer z = scanl (*) 1
    [ z / fromInteger n
    | n <- [1..]
    ]

-- conjecture:  There is some f (independent of a) for which the following is true:
--     exp (CD a b) = CD (exp a) 0 * f b
-- or perhaps something like:
--      exp (CD a b) = CD (exp a) (cos b) * sin b
instance (Floating a, Conj a) => Floating (CD a) where
    pi = CD pi 0
    -- A trial-and-error sort of attempt at generalizing Euler's formula.
    -- Doesn't give right answers, but i'm not sure whether that's because this
    -- is wrong or because cos and sin are (probably both...)
    exp (CD a b) = CD (exp a) 0 * CD (cos b) (sin b)
    
    -- more hypotheses:
    -- cos (CD v 0) = 0.5 * (exp (CD 0 v) + exp (CD 0 (negate v)))
    -- sin (CD v 0) = 0.5 * (exp (CD 0 v) - exp (CD 0 (negate v)))
    -- cosh (CD v 0) = 0.5 * CD (exp v + exp (negate v)) 0
    -- sinh (CD v 0) = 0.5 * CD (exp v - exp (negate v)) 0
    
    -- or maybe:
    cos (CD x y) = CD (cos x * cosh y) (negate (sin x * sinh y))
    sin (CD x y) = CD (sin x * cosh y)         (cos x * sinh y)
    
    -- This version needs some type-gymnastics but should be right
    -- cos z@(CD a b) = (cos v * cosh y) *^ 1 - (sin v * sinh y) *^ m
    --     where
    --         v  = realPart a
    --         vv = imagPart a
    --         y = magSq vv
    --          -- or:
    --         (v,y,m) = polarForm z
    --         m = vv / y
    -- sin z = sin(v) cosh(y ) + cos(v) sinh(y )M 
