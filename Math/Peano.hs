{-
 -	"Peano.hs"
 -	(c) 2008 James Cook
 -}

module Peano where

data Nat = Zero | Succ Nat
        deriving (Eq, Ord, Show)

dec Zero        = error "underflow"
dec (Succ n)    = n

nfold z s Zero     = z
nfold z s (Succ n) = s (nfold z s n)

instance Num Nat where
        (+) n = nfold n Succ
        (-) n = nfold n dec
        (*) n = nfold 0 (+n)
        abs = id
        signum Zero = 0
        signum _ = 1
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
                | x < y         = (0, x)
                | otherwise     = (q + 1, r)
                        where   (q,r) = quotRem (x-y) y
        divMod = quotRem
        toInteger = nfold 0 succ

infinity :: Nat
infinity = Succ infinity

factorial :: (Num a) => Nat -> a
factorial = fst . nfold (1,1) (\x -> (fst x * snd x, snd x + 1))