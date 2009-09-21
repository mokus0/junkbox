{-
 -      ``gadt_dependent''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -
 - dependent types using rank-N quantification, GADTs, and
 - continuation-passing style (to avoid escaping types)
 -}
{-# LANGUAGE
    GADTs, EmptyDataDecls, RankNTypes,
    DeriveDataTypeable, ScopedTypeVariables, PatternSignatures,
    TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances,
    FunctionalDependencies, UndecidableInstances
  #-}

module TypeExperiments.Gadt_dependent where

import Data.Typeable
import Prelude hiding (EQ, head, tail, take, reverse, drop, (++), zip, zipWith, splitAt, iterate, min)

data Zero
    deriving Typeable
data Succ t
    deriving Typeable

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three
type Five  = Succ Four
type Six   = Succ Five
type Seven = Succ Six
type Eight = Succ Seven
type Nine  = Succ Eight
type Ten   = Succ Nine

data Nat n where
    Zero :: Nat Zero
    Succ :: (Typeable n, UniqueNat n) => Nat n -> Nat (Succ n)
    deriving (Typeable)

instance Show (Nat n) where
    showsPrec p Zero = showString "Zero"
    showsPrec p (Succ n) = showParen (p > 10) (showString "Succ " . showsPrec 11 n)

withNum :: Integer -> (forall n. Nat n -> t) -> t
withNum 0 f = f Zero
withNum (n+1) f = withNum n (\m -> case m of 
    m@Zero      -> f (Succ m)
    m@(Succ n)  -> f (Succ m)
    )

-- some handy sample Nat values:
zero = Zero
one   = Succ zero
two   = Succ one
three = Succ two
four  = Succ three
five  = Succ four
six   = Succ five
seven = Succ six
eight = Succ seven
nine  = Succ eight
ten   = Succ nine


numValue :: Num t => Nat n -> t
numValue Zero = 0
numValue (Succ n) = (1 +) $! numValue n

data EQ n m
    where
        EQ :: EQ a a

zeroEQ :: EQ Zero Zero
zeroEQ = EQ

succEQ :: EQ n m -> EQ (Succ n) (Succ m)
succEQ (EQ :: EQ n m) = (EQ :: EQ (Succ n) (Succ m))


data LT n m
    where
       ZeroLT :: UniqueNat n => LT Zero (Succ n)
       SuccLT :: (UniqueNat n, UniqueNat m) => LT n m -> LT (Succ n) (Succ m)

instance Show (LT n m) where
    showsPrec p ZeroLT = showString "ZeroLT"
    showsPrec p (SuccLT proof) = showParen (p > 10) (showString "SuccLT " . showsPrec 11 proof)

type GT n m = LT m n

class Typeable n => UniqueNat n where
    uniqueNat :: Nat n

instance UniqueNat Zero where
    uniqueNat = Zero

instance UniqueNat n => UniqueNat (Succ n) where
    uniqueNat = Succ uniqueNat

-- class UniqueNat (Succ n) => UniquePos n
uniquePred :: UniqueNat (Succ n) => Nat n
uniquePred = pred uniqueNat
    where
        pred :: Nat (Succ n) -> Nat n
        pred (Succ n) = n

decidable_equality :: (EQ n m -> t) -> t -> Nat n -> Nat m -> t
decidable_equality eq ne  Zero     Zero    = eq (EQ :: EQ Zero Zero)
decidable_equality eq ne  Zero     _       = ne
decidable_equality eq ne  _        Zero    = ne
decidable_equality eq ne (Succ n) (Succ m) = decidable_equality eq' ne n m
    where eq' eqWitness = eq (succEQ eqWitness)

trichotomy :: (LT n m -> t) -> (EQ n m -> t) -> (GT n m -> t) -> Nat n -> Nat m -> t
trichotomy lt eq gt Zero                Zero                = eq eqProof
trichotomy lt eq gt Zero                (Succ _ :: Nat m)   = lt ltProof
trichotomy lt eq gt (Succ _ :: Nat n)   Zero                = gt ltProof
trichotomy lt eq gt (Succ (n :: Nat n)) (Succ (m :: Nat m)) = trichotomy lt' eq' gt' n m
    where
        lt' ltWitness = lt (SuccLT ltWitness)
        eq' eqWitness = eq (succEQ eqWitness)
        gt' ltWitness = gt (SuccLT ltWitness)

type LTE n m = Either (EQ n m) (LT n m)

class ProvablyEQ n m where
    eqProof :: EQ n m

instance ProvablyEQ n n where eqProof = EQ
instance ProvablyEQ n m => ProvablyEQ (f n) (f m) where 
    eqProof = case eqProof :: EQ n m of
        EQ -> EQ

class ProvablyLT n m where
    ltProof :: LT n m

instance UniqueNat n => ProvablyLT Zero (Succ n) where
    ltProof = ZeroLT

instance (UniqueNat n, UniqueNat m, ProvablyLT n m) => ProvablyLT (Succ n) (Succ m) where
    ltProof = SuccLT ltProof

withLT :: LT n m -> (forall x. LT x m -> t) -> t
withLT ltProof lt = lt ltProof

-- a non-zero finite ordinal consists of one of:
--  the next smaller ordinal itself
--  an element of the next smaller ordinal
data Ordinal n where
    OrdZero :: UniqueNat n => Ordinal (Succ n)
    OrdSucc :: UniqueNat n => Ordinal n -> Ordinal (Succ n)
    deriving Typeable

instance Show (Ordinal n) where
    showsPrec p (OrdZero) = showParen (p > 10) (showString "OrdZero ")
    showsPrec p (OrdSucc n) = showParen (p > 10) (showString "OrdSucc " . showsPrec 11 n)

ordValue :: Ordinal n -> Integer
ordValue OrdZero = 0
ordValue (OrdSucc n) = (1 +) $! ordValue n

cardinality :: Ordinal n -> Nat n
cardinality OrdZero = uniqueNat
cardinality (OrdSucc n) = Succ (cardinality n)

mkOrdinal :: ProvablyLT n m => Nat n -> Ordinal m
mkOrdinal n = mkOrdinalProof ltProof n
    where
        mkOrdinalProof :: LT n m -> Nat n -> Ordinal m
        mkOrdinalProof  ZeroLT           Zero    = OrdZero
        mkOrdinalProof (SuccLT ltProof) (Succ n) = OrdSucc (mkOrdinalProof ltProof n)

data Vec n a where
    Nil :: Vec Zero a
    Cons :: a -> Vec n a -> Vec (Succ n) a
    deriving Typeable

instance Functor (Vec n) where
    fmap f Nil = Nil
    fmap f (Cons a v) = Cons (f a) (fmap f v)

instance Show a => Show (Vec n a) where
    showsPrec p = showsPrec p . vecToList

v :: a -> Vec One a
v x = Cons x Nil

vecToList :: Vec n a -> [a]
vecToList Nil = []
vecToList (Cons a v) = a : vecToList v

(!) :: Vec n a -> Ordinal n -> a
Cons a v ! OrdZero = a
Cons a v ! OrdSucc n = v ! n

head :: Vec (Succ n) a -> a
head (Cons a v) = a
tail :: Vec (Succ n) a -> Vec n a
tail (Cons a v) = v
take :: ProvablyLT n (Succ m) => Nat n -> Vec m a -> Vec n a
take = takeWithProof ltProof
    where 
        takeWithProof :: LT n (Succ m) -> Nat n -> Vec m a -> Vec n a
        takeWithProof ZeroLT Zero v = Nil
        takeWithProof (SuccLT ltProof) (Succ n) (Cons a v) = Cons a (takeWithProof ltProof n v)

snoc :: Vec n a -> a -> Vec (Succ n) a
snoc Nil a = Cons a Nil
snoc (Cons x v) a = Cons x (snoc v a)

reverse :: Vec n a -> Vec n a
reverse Nil = Nil
reverse (Cons a v) = snoc (reverse v) a

data Sum n m s where
    ZeroSum :: Sum Zero m m
    SuccSum :: Sum n m s -> Sum (Succ n) m (Succ s)
    deriving Typeable

class Add n m s | m n -> s, n s -> m
    where
        addProof :: Sum n m s
instance Add Zero n n
    where addProof = ZeroSum
instance Add n m s => Add (Succ n) m (Succ s)
    where addProof = SuccSum addProof

drop :: Add n d m => Nat n -> Vec m a -> Vec d a
drop = dropWithProof addProof
    where
        dropWithProof :: Sum n d m -> Nat n -> Vec m a -> Vec d a
        dropWithProof ZeroSum Zero v = v
        dropWithProof (SuccSum addProof) (Succ n) (Cons _ v) = dropWithProof addProof n v

splitAt :: Add n d m => Nat n -> Vec m a -> (Vec n a, Vec d a)
splitAt = splitWithProof addProof
    where
        splitWithProof :: Sum n d m -> Nat n -> Vec m a -> (Vec n a, Vec d a)
        splitWithProof ZeroSum Zero v = (Nil, v)
        splitWithProof (SuccSum addProof) (Succ n) (Cons a v) = case splitWithProof addProof n v of
            (pre, post) -> (Cons a pre, post)

(++) :: Add n m s => Vec n a -> Vec m a -> Vec s a
(++) = appendWithProof addProof
    where
        appendWithProof :: Sum n m s -> Vec n a -> Vec m a -> Vec s a
        appendWithProof ZeroSum Nil v = v
        appendWithProof (SuccSum addProof) (Cons a v1) v2 = Cons a (appendWithProof addProof v1 v2)

zip :: Vec n a -> Vec n b -> Vec n (a,b)
zip = zipWith (,)

zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith (*) Nil Nil = Nil
zipWith (*) (Cons a v1) (Cons b v2) = Cons (a*b) (zipWith (*) v1 v2)

-- can this be written it a way that it will actually execute?
-- I suspect not...
iterate :: (a -> a) -> a -> (forall n. Vec n a -> t) -> t
iterate f x g = iterate f (f x) (\v -> g (Cons x v))

foldVec :: t -> (a -> t -> t) -> Vec n a -> t
foldVec nil cons Nil = nil
foldVec nil cons (Cons x v) = cons x (foldVec nil cons v)

data Base b where
    BNil  :: UniqueNat b => Base b
    BCons :: UniqueNat b => Ordinal b -> Base b -> Base b
    deriving Typeable

instance Show (Base b) where
    showsPrec p b@BNil = showString "{base " . shows (numValue (base b)) . showString "}"
    showsPrec p (BCons d b) = showDigit d . shows b

showDigit d = showParen (numValue (cardinality d) > 10) (shows (ordValue d))

base :: Base b -> Nat b
base BNil = uniqueNat
base (BCons _ _) = uniqueNat

data Void

type Not a = a -> Void

data If p t f where
    T :: p       -> t -> If p t f
    F :: (Not p) -> f -> If p t f

instance (Show p, Show t, Show f) => Show (If p t f) where
    showsPrec p (T proof t) = showParen (p > 10) (showString "T " . showsPrec 11 proof . showChar ' ' . showsPrec 11 t)
    showsPrec p (F proof f) = showParen (p > 10) (showString "F _ " . showsPrec 11 f)

min :: Nat a -> Nat b -> If (LT a b) (Nat a) (Nat b)
min Zero  Zero    = F (\_ -> error "impossible proof") Zero
min Zero (Succ _) = T ltProof Zero
min (Succ _) Zero = F (\_ -> error "impossible proof") Zero
min (Succ a) (Succ b) = case min a b of
    T ltProof  x -> T (SuccLT ltProof) (Succ x)
    F nltProof x -> F (\(SuccLT pr) -> nltProof pr) (Succ x)

(?) :: If p t f -> (t -> a, f -> a) -> a
T p x ? (f,g) = f x
F p x ? (f,g) = g x

(??) :: If p t f -> (p -> t -> a, Not p -> f -> a) -> a
T p x ?? (f,g) = f p x
F p x ?? (f,g) = g p x
