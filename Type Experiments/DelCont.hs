module DelCont where

-- overriding monad operators to allow use of do-notation with looser types
-- (requires NoImplicitPrelude in code using them)
import Prelude hiding (return, (>>=), (>>), fail)

newtype CC ans a b t =
    CC { runCC :: Ctn ans t a
               -> Sctx ans b
               -> ans
        -- could also add a dummy ()-> on last ans, to make the whole
        -- type: Fun () a t b ans
        -- AKA: ()/a -> t/b
       }

type Sctx ans val = val -> ans
type Ctn ans s a = Sctx ans a -> Sctx ans s
-- Danvy & Filinski's s/a -> t/b, I think
-- (might have switched a and b, I never could keep those straight)
type Fun ans s a t b = Ctn ans t a -> Ctn ans s b

eval x = runCC x id id

return x = CC $ \k gam -> k gam x
shift  f = CC $ \k gam -> runCC (f (resetTo k)) id gam
shift' f = CC $ \k gam -> runCC (f (\x -> CC (\k' gam' -> k (k' gam') x))) id gam
abort  x = CC $ \k gam -> runCC x id gam

reset x = resetTo id x
resetTo f x = CC $ \k -> runCC x f . k

lift :: (s -> CC ans a b t) -> Fun ans s a t b
lift f k = ctn f k -- . k

ctn f k gam val = runCC (f val) k gam

infixl 1 >>=, >>
(>>=) :: CC ans b c s -> (s -> CC ans a b t) -> CC ans a c t
x >>= f = CC $ \k gam -> runCC x (lift f k) gam
x >> y = x >>= const y

fail = abort . return
