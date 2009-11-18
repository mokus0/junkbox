{-# -fno-monomorphism-restriction #-}
{-
 -      ``Expr.hs''
 -      (c) 2008 James Cook
 -}

module Monad where

import Control.Monad
import Control.Monad.Instances

-- some experiments with monads, to test my understanding...
-- a simple lambda-calculator:

-- "primed" variable names, used to handle variable name clash in a
-- type-natural way (whenever a name is shadowed, the hidden one
-- gets "primed" with the S constructor... i think... this comment
-- was written long after the original code)
data Prime k 
        = Z k
        | S (Prime k)
        deriving (Eq, Show)

(  Z x) `isPrefixOf` (Z y)      = (x == y)
x@(Z _) `isPrefixOf` (S y)      = x `isPrefixOf` y
_       `isPrefixOf` _          = False

instance Functor Prime where
        fmap f (Z k) = Z (f k)
        fmap f (S k) = S (fmap f k)

instance Monad Prime where
        (Z k) >>= f = f k
        (S k) >>= f = S (k >>= f)
        return = Z

data Expr k f
        = Free f
        | Bound (Prime k)
        | Lam (Prime k) (Expr k f)
        | App (Expr k f) (Expr k f)
        deriving (Eq, Show)

instance Functor (Expr k) where
        fmap f (Free x)         = Free (f x)
        fmap f (Bound k)        = Bound k
        fmap f (Lam k x)        = Lam k (fmap f x)
        fmap f (App x1 x2)      = App (fmap f x1) (fmap f x2)

bmap :: (Prime a -> Prime b) -> Expr a c -> Expr b c
bmap f (Free x)                 = Free x
bmap f (Bound x)                = Bound (f x)
bmap f (Lam k x)                = Lam (f k) (bmap f x)
bmap f (App x1 x2)              = App (bmap f x1) (bmap f x2)

bjoin = bmap join

instance (Eq k) => Monad (Expr k) where
        (Free x) >>= f          = f x
        (Bound k) >>= f         = Bound k
        (Lam k x) >>= f         = Lam k (x >>= (fmap (bmap (prime k)) f))
        (App x1 x2) >>= f       = App (x1 >>= f) (x2 >>= f)
        return x                = Free x

unbind :: Prime String -> Expr String String -> Expr String String
unbind v (Free x)
        | (unPrime v) == x              = error "unbind: name clash"
unbind v (Bound k)
        | k == v                        = Free (unPrime k)
unbind v (Lam k x)
        | v == k                        = Lam k x
        | otherwise                     = Lam k (unbind v x)
unbind v (App x1 x2)                    = App (unbind v x1) (unbind v x2)
unbind v x                              = x

subst a b = \x -> if x == a then b else return x

unPrime (Z x) = x
unPrime (S x) = (unPrime x) ++ "'"

prime k x
        | k `isPrefixOf` x      = S x
        | otherwise             = x

reduce :: Expr String String -> Expr String String
reduce (App (Lam k x) y) = join (fmap (subst (unPrime k) y) (unbind k x))
reduce (App x1 x2) = App (reduce x1) (reduce x2)
reduce (Lam k x) = Lam k (reduce x)
reduce x = x

lam x e = Lam (Z x) e
bound x = Bound (Z x)

k = lam "x" (lam "_" (bound "x"))
s = lam "x" (lam "y" (lam "z" (App (App (bound "x") (bound "z")) (App (bound "y") (bound "z")))))
i = App (App (Free "s") (Free "k")) (Free "k")
y =     lam "f"
                (lam "x" 
                        (App 
                                (App (bound "f") (App (bound "x") (bound "x")))
                                (App (bound "f") (App (bound "x") (bound "x")))
                        )
                )

app = App
app2 f x y = App (App f x) y
y2 = app2 s
        (App k (app2 s i i))
        (app2 s
                (app2 s (App k s) s)
                (App k (app2 s i i))
        )

y3 = app
        (app2 s s k)
        (app2 s
                (app k
                        (app2 s
                                s
                                (app s
                                        (app2 s s k)
                                )
                        )
                )
                k
        )

