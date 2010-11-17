{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Monads.Weighted where

import Control.Monad
import Control.Monad.Instances ()
import Data.Monoid
import Data.Complex
import qualified Data.Map as M
import Data.Either

newtype W w t = W { toWeightedList :: [(w,t)] }
    deriving (Eq, Ord, Show)

fromWeightedList :: [(w,t)] -> W w t
fromWeightedList = W

instance Functor (W w) where
    fmap f (W xs) = W (map (fmap f) xs)

instance Monoid w => Monad (W w) where
    return x = W [(mempty, x)]
    fail x = W []
    W xs >>= f = W
        [ (w `mappend` w', y)
        | (w,x) <- xs
        , (w', y) <- toWeightedList (f x) 
        ]

instance Monoid w => MonadPlus (W w) where
    mzero = W []
    mplus (W xs) (W ys) = W (xs ++ ys)

instance (Monoid w, Monoid t) => Monoid (W w t) where
    mempty = W []
    mappend = liftM2 mappend

newtype P a = P a
    deriving (Eq, Ord, Show, Num, Fractional, Integral, Real, RealFrac, Floating, RealFloat, Enum)

instance Num a => Monoid (P a) where
    mempty = 1
    mappend = (*)

newtype Q a = Q (Complex a)
    deriving (Eq, Show, Num, Fractional, Floating)

instance RealFloat a => Monoid (Q a) where
    mempty = 1
    mappend = (*)


swap (a,b) = (b,a)
collectBy (+) = W . map swap . M.toList . M.fromListWith (+) . map swap . toWeightedList

collect :: (Num w, Ord t) => W w t -> W w t
collect = collectBy (+)

mapW f (W xs) = W (map (\(w,x) -> (f w, x)) xs)
infixl 7 .*
a .* b  = mapW (a *) b 
infixl 6 .+
a .+ b = mplus a b
infixl 6 .-
a .- b = a .+ (-1) .* b

type Bit = Bool
type PBit w = W (P w) Bit
type QBit w = W (Q w) Bit

rotate :: RealFloat w => w -> Bit -> QBit w
rotate theta True = 
    let theta' = Q (theta :+ 0)
     in cos (theta'/2) .* return True .- sin (theta'/2) .* return False
rotate theta False =
    let theta' = Q (theta :+ 0)
     in cos (theta'/2) .* return True .+ sin (theta'/2) .* return False

hadamard :: RealFloat w => QBit w -> QBit w
hadamard x = x >>= rotate(pi/2)

observe :: RealFloat w => QBit w -> PBit w
observe (W xs) = W
    [ (P (magnitude (q*q)), x)
    | (Q q, x) <- xs
    ]

zeno n = foldr (>=>) return (replicate n (rotate (1/fromIntegral n)))

qJoin :: RealFloat w => W (P w) (W (P w) a) -> W (Q w) a
qJoin xss = W
    [ (Q (a :+ b), x)
    | (P a, xs) <- toWeightedList xss
    , (P b,  x) <- toWeightedList xs
    ]

class Normalize w where
    normalize :: W w a -> W w a

instance Fractional w => Normalize (P w) where
    normalize (W xs) = W [(w / tot, x) | (w,x) <- xs]
        where tot = sum (map fst xs)

separate (W xs) = (normalize (W l), normalize (W r))
    where 
        (l,r) = partitionEithers (map liftE xs)
        liftE (w, Left  x) = Left  (w, x)
        liftE (w, Right x) = Right (w, x)
        