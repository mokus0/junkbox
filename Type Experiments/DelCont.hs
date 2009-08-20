module DelCont where

-- overriding monad operators to allow use of do-notation with looser types
-- (requires NoImplicitPrelude in code using them)
import Prelude hiding (return, (>>=), (>>), fail)

newtype CC a b t =
    CC { runCC :: (t -> b) -> a }

eval x = runCC x id
reset = resetTo id
resetTo f x = CC (\k -> k (runCC x f))

return x = CC (\k -> k x)
shift  f = CC $ \k -> runCC (f (resetTo k)) id
shift' f = CC $ \k -> runCC (f (\x -> CC (\sk -> sk (k x)))) id
abort  x = CC $ \k -> runCC x id

infixl 1 >>=, >>, >=>
(>>=) :: CC a b s -> (s -> CC b c t) -> CC a c t
x >>= f = CC $ \k -> runCC x (lift f k)
x >> y = x >>= const y

lift :: (s -> CC b c t) -> (t -> c) -> s -> b
lift f k val = runCC (f val) k

-- ok, this is how I think the order of a and b in s/a -> t/b is chosen:
-- (at least, it sure would make a lot of sense)
type Fun s a t b = s -> CC a b t
(>=>) :: Fun s a t b -> Fun t b u c -> Fun s a u c
f >=> g = \x -> f x >>= g

fail = abort . return
