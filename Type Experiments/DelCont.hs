{-# LANGUAGE RankNTypes #-}
module DelCont where

-- overriding monad operators to allow use of do-notation with looser types
-- (requires NoImplicitPrelude in code using them)
import Prelude hiding (return, (>>=), (>>), fail)

newtype CC a b t =
    CC { runCC :: (t -> b) -> a }

-- eval :: (1/a -> t/t) -> a
-- Note that this relaxes the top-level type from (CC t t t -> t),
-- creating an implied reset at the top level.  The only difference
-- is the typing:
--        eval (reset x)
--      = runCC (reset x) id
--      = runCC (resetTo id x) id
--      = runCC (CC (\k -> k (runCC x id))) id
--      = (\k -> k (runCC x id)) id
--      = id (runCC x id)
--      = runCC x id
--      = eval x {- modulo typing -}
eval :: CC a t t -> a
eval x = runCC x id

-- reset :: (1/s -> t/t) -> (1/b -> s/b)
reset :: CC s t t -> CC b b s
reset = resetTo id

-- resetTo :: (t -> b) -> (1/s -> t/b) -> (1/a -> s/a)
resetTo :: (t -> b) -> CC s b t -> CC a a s
resetTo f x = CC (\k -> k (runCC x f))

-- return :: t/a -> t/a
-- or     :: t -> (1/a -> t/a)
return :: t -> CC a a t
return x = CC (\k -> k x)

-- these types were not very intuitive to me until I realized
-- how it is that in (1/a -> u/u) below, despite the fact that
-- u/u occurs nowhere else in the type of shift, the result of the
-- shifted function still manages to "escape".
-- The "a" is the type of its escape path.  In a "normal" return,
-- eg, "let f k = do ...; return (_::t) in shift f", the domain
-- of f will  be something like (1/a -> t/a).  The u/u (caused by
-- an implied reset) will unify t with a, giving (1/t -> t/t).
-- Failure to unify in this way indicates that the shifted function
-- never returns normally (ie, that it always either tail-calls or
-- discards the continuation)
-- Similarly, in the result type (1/a -> t/s), s is the type of a
-- hidden input that is made available through use of the
-- continuation passed by shift.
-- Note also that the type of the continuation is pure, and can be
-- invoked in multiple subcontexts, or even 'eval'ed.
-- shift :: ((forall b. t/b -> s/b) -> (1/a -> u/u)) -> (1/a -> t/s)

shift :: ((forall b. t -> CC b b s) -> CC a u u) -> CC a s t
shift  f = CC $ \k -> runCC (f (\x -> CC (\sk -> sk (k x)))) id

-- shift' :: (((1/v -> t/s) -> (1/b -> v/b)) -> (1/a -> u/u)) -> (1/a -> t/s)
shift' :: ((CC v s t -> CC b b v) -> CC a u u) -> CC a s t
shift' f = CC $ \k -> runCC (f (resetTo k)) id

shift'' :: ((t -> s) -> CC a u u) -> CC a s t
shift'' f = CC $ \k -> runCC (f k) id

-- abort :: (1/a -> u/u) -> (1/a -> t/b)
abort :: CC a u u -> CC a b t
abort  x = CC $ \k -> runCC x id

infixl 1 >>=, >>, >=>
-- (>>=) :: (1/a -> s/b) -> (s/b -> t/c) -> (1/a -> t/c)
(>>=) :: CC a b s -> (s -> CC b c t) -> CC a c t
x >>= f = CC $ \k -> runCC x (\val -> runCC (f val) k)
x >> y = x >>= const y

-- ok, this is how I think the order of a and b in s/a -> t/b is chosen:
-- (at least, it sure would make a lot of sense)
type Fun s a t b = s -> CC a b t
(>=>) :: Fun s a t b -> Fun t b u c -> Fun s a u c
f >=> g = \x -> f x >>= g

fail = abort . return
