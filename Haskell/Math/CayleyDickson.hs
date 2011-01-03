{-# LANGUAGE
        MultiParamTypeClasses,
        FunctionalDependencies,
        FlexibleInstances,
        UndecidableInstances,
        TypeSynonymInstances
  #-}
module Math.CayleyDickson where

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
    recip x@(CD a b) = CD (conj a / magSq) (negate b / magSq)
        where magSq = a*conj a + conj b * b
    
    x / y = x * recip y

x \\ y = recip x * y
infixr 7 \\