{-# LANGUAGE RankNTypes, FlexibleInstances #-}
module DelCont where

-- overriding monad operators to allow use of do-notation with looser types
-- (requires NoImplicitPrelude in code using them)
import Prelude hiding (return, (>>=), (>>), fail, abs)

newtype CC a b t =
    CC { runCC :: (t -> a) -> b }

-- eval :: (1/t -> t/a) -> a
eval :: CC t a t -> a
eval x = runCC x id

-- reset :: (1/t -> t/s) -> (1/b -> s/b)
reset :: CC t s t -> CC b b s
reset = resetTo id

-- resetTo :: (t -> b) -> (1/b -> t/s) -> (1/a -> s/a)
resetTo :: (t -> b) -> CC b s t -> CC a a s
resetTo f x = CC (\k -> k (runCC x f))

-- return :: t/a -> t/a
-- or     :: t -> (1/a -> t/a)
return :: t -> CC a a t
return x = CC (\k -> k x)

-- shift :: ((forall b. t/b -> s/b) -> (1/u -> u/a)) -> (1/s -> t/a)
shift :: ((t -> CC b b s) -> CC u a u) -> CC s a t
shift  f = CC $ \k -> runCC (f (\x -> CC (\sk -> sk (k x)))) id

-- shift' :: (((1/s -> t/v) -> (1/b -> v/b)) -> (1/u -> u/a)) -> (1/s -> t/a)
shift' :: ((CC s v t -> CC b b v) -> CC u a u) -> CC s a t
shift' f = CC $ \k -> runCC (f (resetTo k)) id

shift'' :: ((t -> s) -> CC u a u) -> CC s a t
shift'' f = CC $ \k -> runCC (f k) id

-- abort :: (1/u -> u/a) -> (1/b -> t/a)
abort :: CC u a u -> CC b a t
abort  x = CC $ \k -> runCC x id

infixl 1 >>=, >>, >=>
-- (>>=) :: (1/b -> s/a) -> (s/c -> t/b) -> (1/c -> t/a)
(>>=) :: CC b a s -> (s -> CC c b t) -> CC c a t
x >>= f = CC $ \k -> runCC x (\val -> runCC (f val) k)
x >> y = x >>= const y

-- type Fun s a t b = s/a -> t/b
type Fun s a t b = s -> CC a b t
-- (>=>) :: (s/b -> t/a) -> (t/c -> u/b) -> (s/c -> u/a)
(>=>) :: Fun s b t a -> Fun t c u b -> Fun s c u a
f >=> g = \x -> f x >>= g

-- fail :: a/b -> t/a
fail :: Fun a b t a
fail x = abort (return x)

-- control abstraction and application
abs :: (Fun a t t s) -> CC b b (Fun a t s t)
abs f = reset (shift return >>= f)

app :: CC t s (Fun a b c t) -> Fun a b c s
app f x = f >>= \f -> f x
