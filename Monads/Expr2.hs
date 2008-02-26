{-# OPTIONS -fno-monomorphism-restriction #-}
{-
 -      ``Expr2.hs''
 -      (c) 2008 James Cook
 -}

module Expr2 where

import Prelude hiding (($))
import Triple hiding (fold)
import Data.Maybe

data Expr k v
        = Const k
        | Bound Int
        | Free v
        | Lam (Expr k v)
        | App (Expr k v) [Expr k v]
        deriving (Eq, Show)

fold f1 f2 f3 f4 f5 (Const k)           = f1 k
fold f1 f2 f3 f4 f5 (Bound i)           = f2 i
fold f1 f2 f3 f4 f5 (Free v)            = f3 v
fold f1 f2 f3 f4 f5 (Lam e)             = f4 (fold f1 f2 f3 f4 f5 e)
fold f1 f2 f3 f4 f5 (App e xs)          = f5 (fold f1 f2 f3 f4 f5 e) (map (fold f1 f2 f3 f4 f5) xs)

instance Functor (Expr k) where
        fmap f = fold Const Bound (Free . f) Lam App

instance Triple (Expr k) where
        eta                     = Free
        
        etaInv (Free e)         = Just e
        etaInv (App e [])       = etaInv e
        etaInv _                = Nothing
        
        mu = fold Const Bound id Lam App

-- these don't fold easily, because they track the binding-depth. bummer...
unbind n expr
        | n < 0                 = fmap Left expr
unbind n (Bound m)              = case m `compare` n of
        LT      -> Bound m
        EQ      -> Free (Right m)
        GT      -> Bound (m-1)
unbind n (Free fv)              = Free (Left fv)
unbind n (Lam e)                = Lam (unbind (n+1) e)
unbind n (App e xs)             = App (unbind n e) (map (unbind n) xs)

bind (Const k)  n d             = Const k
bind (Bound i)  n d 
        | i >= n                = Bound (i+d)
        | otherwise             = Bound i
bind (Free v)   n d             = Free v
bind (Lam e)    n d             = Lam (bind e (n+1) d)
bind (App e xs) n d             = App (bind e n d) (map (\x -> bind x n d) xs)

subst fv x v
        | v == fv       = x
        | otherwise     = return v

apply expr arg = do
        fv <- unbind 0 expr
        either return (bind arg 0) fv

-- outermost evaluation
reduce (App e [])               = e
reduce (App (Lam e) [a])        = apply e a
reduce (App (Lam e) (a:as))     = App (apply e a) as
reduce (App e xs)               = App (reduce e) (map reduce xs)
reduce (Lam e)                  = Lam (reduce e)
reduce x                        = x

-- reduce to normal form, if possible
reduce' e 
        | e == e'               = e
        | otherwise             = reduce' (e')
        where e' = reduce e

-- some handy constructors
infixl 9 $>
x $> y = App x [y]

infixr 0 <$
x <$ y = App x [y]

k' = Free "k"
k = Lam (Lam (Bound 1))

s' = Free "s"
s = Lam (Lam (Lam (App (App (Bound 2) [Bound 0]) [App (Bound 1) [Bound 0]])))

i'' = Free "i"
i' = s' $> k' $> k'
i = i' >>= subst "k" k >>= subst "s" s

y = Lam (Lam (App g [g]))
        where g = App (Bound 1) [Bound 0, Bound 0]

-- this version diverges under our evaluation strategy
-- y'' = Free "y"
-- y' = App s' [ App k' [App s' [i',i']]
--             , App s' [ App s' [App k' [s'], s']
--                      , App k' [App s' [i', i']]
--                      ]
--             ]
-- y = y' >>= subst "i" i' >>= subst "s" s >>= subst "k" k