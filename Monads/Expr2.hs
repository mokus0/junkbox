{-# OPTIONS -fno-monomorphism-restriction #-}
{-
 -      ``Expr2.hs''
 -      (c) 2008 James Cook
 -}

module Expr2 where

import Prelude hiding (succ, pred, exp)
import Triple hiding (fold)
import Data.Maybe

data Expr k v where
        Const   :: k -> Expr k v
        Bound   :: Int -> Expr k v
        Free    :: v -> Expr k v
        Lam     :: Expr k v -> Expr k v
        App     :: Expr k v -> [Expr k v] -> Expr k v
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
lam x e = Lam (bind' 0 e)
        where 
                bind' d (Const k)       = Const k
                bind' d (Bound i)       = Bound i
                bind' d (Free v)
                        | v == x        = Bound d
                        | otherwise     = Free v
                bind' d (Lam e)         = Lam (bind' (d+1) e)
                bind' d (App e as)      = App (bind' d e) (map (bind' d) as)
              
lambdas = flip (foldr lam)
                

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

-- Y = λf·(λx·f (x x)) (λx·f (x x))
y = lam "f" (g $> g)
        where g = lam "x" (Free "f" $> (Free "x" $> Free "x"))

-- this version diverges under our evaluation strategy
-- (should be: S (K (S I I)) (S (S (K S) K) (K (S I I))))
y2'' = Free "y2"
y2' = App s' [ App k' [sii]
             , App s' [ App s' [App k' [s'], k']
                      , App k' [sii]
                      ]
             ] where sii = App s' [i', i']
y2 = y2' >>= subst "i" i' >>= subst "s" s >>= subst "k" k

-- S S K (S (K (S S (S (S S K)))) K)
-- (also diverges)
y3'' = Free "y3"
y3' = App ssk [ App s' [ App k' [ App s' [s'
                                         , App s' [ssk]
                                         ]
                                ]
                       ]
              , k'
              ] where ssk = App s' [s', k']
y3 = y3' >>= subst "s" s >>= subst "k" k

-- Yk = (L L L L L L L L L L L L L L L L L L L L L L L L L L)
-- L = λabcdefghijklmnopqstuvwxyzr. (r (t h i s i s a f i x e d p o i n t c o m b i n a t o r))
l' = Free "l"
l = lambdas "abcdefghijklmnopqstuvwxyzr" (App (Free "r") (args "thisisafixedpointcombinator"))
        where
                lambdas :: [Char] -> Expr k String -> Expr k String
                lambdas = flip (foldr (lam.return))
                args :: [Char] -> [Expr x String]
                args = fmap (return.return)
               
data A = forall a. Eq a => A a
                
y4'' = Free "y4"
y4' = App l' (replicate 25 (l'))
y4 = y4' >>= subst "l" l

zero = lam "f" (lam "x" (return "x"))
succ = lam "n" (lam "f" (lam "x" (App f [App n [f, x]])))
        where
                f = Free "f"
                n = Free "n"
                x = Free "x"

-- pred ≡ λn.λf.λx. n (λg.λh. h (g f)) (λu. x) (λu. u)
pred = lambdas ["n", "f", "x"] $
        App n [ lambdas ["g", "h"] (h $> (g $> f))
              , lam "u" x
              , lam "u" u
              ] where
                      [n, f, x, g, h, u] = map Free ["n", "f", "x", "g", "h", "u"]

nats = iterate (reduce' . (succ $>)) zero
nat = (nats !!)

plus = lambdas ["m", "n", "f", "x"] (App m [f, App n [f,x]])
        where
                [m,n,f,x] = map Free ["m", "n", "f", "x"]

mult = lambdas ["m", "n", "f"] (App n [App m [f]])
        where
                [m,n,f] = map Free ["m", "n", "f"]

exp = lam "m" (lam "n" (App (Free "n") [Free "m"]))

sub = lambdas ["m", "n"] (exp $> pred $> Free "n" $> Free "m")

true  = lambdas ["x", "y"] (return "x")
false = lambdas ["x", "y"] (return "y")
