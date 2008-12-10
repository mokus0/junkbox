{-
 -	"Peano.hs"
 -	(c) 2008 James Cook
 -}
{-# LANGUAGE
        Rank2Types
  #-}

module Peano where

data Nat = Zero | Succ Nat
        deriving (Eq, Show)

dec Zero        = error "underflow"
dec (Succ n)    = n

nfold z s Zero     = z
nfold z s (Succ n) = s (nfold z s n)

nunfold f x = case f x of
        Nothing -> Zero
        Just y  -> Succ (nunfold f y)

nmux zz sz zs ss = go
        where
                go Zero     Zero     = zz
                go Zero     (Succ y) = zs y   (go Zero y)
                go (Succ x) Zero     = sz x   (go x Zero)
                go (Succ x) (Succ y) = ss x y (go x y)

nzip s z x Zero = z x
nzip s z Zero y = z y
nzip s z (Succ x) (Succ y) = s (nzip s z x y)
-- nzip s z = nmux (z Zero) (\y _ -> z y) (\x _ -> z x) (\_ _ -> s)

nzip2 s z x Zero = z x Zero
nzip2 s z Zero y = z Zero y
nzip2 s z (Succ x) (Succ y) = s (nzip2 s z x y)


c2n :: ChurchNumeral -> Nat
c2n n = n Succ Zero

n2c :: Nat -> ChurchNumeral
n2c n f = nfold id (f.) n

instance Ord Nat where
        compare Zero Zero = EQ
        compare Zero (Succ n) = LT
        compare (Succ n) Zero = GT
        compare (Succ n) (Succ m) = compare n m
        
        min = nzip Succ (const Zero)
        max = nzip Succ id

instance Num Nat where
        (+) n = nfold n Succ
        (-) n = nfold n dec
        (*) n = nfold 0 (+n)
        
        abs = id
        
        signum = nfold Zero (const (Succ Zero))
        
        fromInteger 0 = Zero
        fromInteger (n+1) = Succ (fromInteger n)

instance Enum Nat where
        succ = Succ
        pred = dec
        
        toEnum 0 = Zero
        toEnum (n+1) = Succ (toEnum n)
        
        fromEnum = nfold 0 succ

instance Real Nat where
        toRational = nfold 0 succ

instance Integral Nat where
        quotRem x y
                | x < y         = (Zero, x)
                | otherwise     = (Succ q, r)
                        where   (q,r) = quotRem (x-y) y
        divMod = quotRem
        toInteger = nfold 0 succ

infinity :: Nat
infinity = Succ infinity

factorial :: (Num a) => Nat -> a
factorial = fst . nfold (1,1) (\x -> (fst x * snd x, snd x + 1))

data Inte = I {pos :: Nat, neg :: Nat} deriving Show

normalize i = nzip2 id I (pos i) (neg i)

ifold z s p n = ifoldQ (normalize n)
        where
                ifoldQ (I Zero Zero) = z
                ifoldQ (I n@(Succ _) Zero) = nfold z s n
                ifoldQ (I Zero n@(Succ _)) = nfold z p n

instance Eq Inte where
        (I p1 n1) == (I p2 n2) = (p1 + n2) == (p2 + n1)

instance Ord Inte where
        compare (I p1 n1) (I p2 n2) = compare (p1 + n2) (p2 + n1)

instance Enum Inte where
        -- succ (I p (Succ n)) = I p n
        succ (I p n)        = I (Succ p) n
        
        -- pred (I (Succ p) n) = I p n
        pred (I p n)        = I p (Succ n)
        
        toEnum n = I (toEnum n) Zero
        fromEnum = ifold 0 succ pred . normalize

instance Num Inte where
        fromInteger n
                | n >= 0        = I (fromInteger n) Zero
                | otherwise     = I Zero (fromInteger n)
        
        (+) n = ifold n succ pred
        (-) n = ifold n pred succ
        (*) n = ifold 0 (+n) (subtract n)
        
        abs = n2i . absNat
        signum (I p n) = nzip2 id (\x y -> case compare x y of LT -> -1; EQ -> 0; GT -> 1) p n

n2i n = I n Zero
absNat (I p n) = nzip id id p n

instance Real Inte where
        toRational = ifold 0 succ pred

instance Integral Inte where
        toInteger = ifold 0 succ pred
        quotRem x y = (I q Zero * s, I r Zero * s)      -- not right... also needs separate divMod
                where
                        s = signum x * signum y
                        (q, r) = quotRem (absNat x) (absNat y)

data Inte2 = P Nat | N Nat deriving (Eq, Show)

instance Ord Inte2 where
        compare (P a) (P b) = compare a b
        compare (P Zero) (N Zero) = EQ
        compare (P a) (N b) = GT
        
        compare (N a) (N b) = compare b a
        compare (N Zero) (P Zero) = EQ
        compare (N a) (P b) = LT

type ChurchNumeral = forall a. (a -> a) -> (a -> a)
newtype CN = CN { unCN :: ChurchNumeral }

instance Enum CN where
        succ (CN n) = CN (\f -> f . n f)
        
        toEnum 0 = CN (const id)
        toEnum (n+1) = succ (toEnum n)
        
        fromEnum (CN n) = n succ 0

instance Eq CN where
        (CN a) == (CN b) = a succ 0 == (b succ 0 :: Nat)

instance Ord CN where
        (CN a) `compare` (CN b) = a succ 0 `compare` (b succ 0 :: Nat)

instance Show CN where
        showsPrec p (CN a) = showsPrec p (a succ 0)

instance Num CN where
        fromInteger 0 = CN (const id)
        fromInteger (n+1) = succ (fromInteger n)
        
        (CN a) + (CN b) = CN (\f -> a f . b f)
        (CN a) * (CN b) = CN (a . b)
        
        signum x = if x == 0 then 0 else 1
        abs x = x * signum x

