{-# LANGUAGE ImplicitPrelude #-}
module TypeExperiments.Semiring where

import qualified Prelude as P
import Prelude (Eq(..), Ord(..), Functor(..), Monad(..), otherwise)
import qualified Data.Set as S
import Control.Applicative

data Semiring r = Semiring
    { zero  :: r
    , one   :: r
    , add   :: r -> r -> r
    , mult  :: r -> r -> r
    }

bool = Semiring
    { zero  = P.False
    , one   = P.True
    , add   = (P.||)
    , mult  = (P.&&)
    }

num = Semiring
    { zero  = 0
    , one   = 1
    , add   = (P.+)
    , mult  = (P.*)
    }

int = num :: Semiring P.Int

set base = Semiring
    { zero  = S.singleton (zero base)
    , one   = S.singleton (one base)
    , add   = \xs ys -> S.fromList [add  base x y | x <- S.toList xs, y <- S.toList ys]
    , mult  = \xs ys -> S.fromList [mult base x y | x <- S.toList xs, y <- S.toList ys]
    }

pair r1 r2 = Semiring
    { zero  = (zero r1, zero r2)
    , one   = (one  r1, one  r2)
    , add   = \(x1,x2) (y1,y2) -> (add  r1 x1 y1, add  r2 x2 y2)
    , mult  = \(x1,x2) (y1,y2) -> (mult r1 x1 y1, mult r2 x2 y2)
    }

applicative r = Semiring
    { zero  = pure (zero r)
    , one   = pure (one  r)
    , add   = liftA2 (add  r)   -- not valid actually (not commutative)
    , mult  = liftA2 (mult r)
    }

infixl 6 +
x + y = add <*> x <*> y

infixl 7 *
x * y = mult <*> x <*> y

fromInteger 0 = zero
fromInteger 1 = one
fromInteger n
    | P.even n  = fromInteger n' + fromInteger n'
    | otherwise = fromInteger (n P.- 1) + one
        where n' = P.div n 2

s || t  = \r -> S.union        (s r) (t r)
s && t  = \r -> S.intersection (s r) (t r)