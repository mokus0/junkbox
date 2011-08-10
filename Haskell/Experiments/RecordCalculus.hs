{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- mostly just some experimenst with 'Unbound'...
module Experiments.RecordCalculus where

import Unbound.LocallyNameless

-- A simple untyped lambda calculus, with access to "primitive" values, 
-- extended to include "case" constructs, with the following semantics:
-- 1. A new primitive value type, the "selector".  Selectors are primitive
--    values, the only significance of which is their identity.
-- 2. A case expression is a selector-indexed tuple: a finite map from selectors
--    to expressions.  Application of a case expression to a selector yields
--    an expression iff the case contains an expression for that selector or
--    a default case (default cases are not mandatory).
-- (code has evolved away from that idea, but it was interesting and is worth
-- returning to and exploring, I think)

data Prim
    = Num Integer
    | Str String
    | PrimOp String
    deriving Show

newtype Sel = Sel Integer
    deriving (Eq, Ord, Num, Enum, Real, Integral)
instance Show Sel where
    showsPrec p (Sel n) = showsPrec p n

data Pat
    = VarP (Name Expr)
    | SelP Sel
    | RecP [Pat]
    deriving (Eq, Show)

data Expr
    = Prim Prim
    | VarE (Name Expr)
    | SelE Sel
    | Abs (Bind [Pat] Expr)
    | RecE [Expr]
    | Case [Bind Pat Expr]
    | App Expr Expr
    deriving Show
instance Eq Expr where (==) = aeq

$(derive [''Expr, ''Prim, ''Sel, ''Pat])
instance Alpha      Pat
instance Subst Expr Pat
instance Alpha      Prim
instance Subst Expr Prim
instance Alpha      Sel
instance Subst Expr Sel
instance Alpha      Expr
instance Subst Expr Expr where
  isvar (VarE v) = Just (SubstName v)
  isvar _       = Nothing

num = Prim . Num
str = Prim . Str
primOp = Prim . PrimOp
--lam x = Abs . bind (map (VarP . s2n) x)
lam x e = Case [bind (VarP (s2n x)) e]
lams []     e = e
lams (x:xs) e = lam x (lams xs e)
var = VarE . s2n
app = App
apps = foldl1 app

-- s = \f -> (\g -> (\x -> app (app f x) (app g x)))
s = lams ["f", "g", "x"] (apps [f,x,app g x])
    where
        f = var "f"
        g = var "g"
        x = var "x"
k = lams ["x", "y"] (var "x")
i = lam "x" (var "x")
sel = SelE
cse c = Case (map (uncurry bind) c)

reduce :: Fresh m => Expr -> m Expr
reduce (Abs b) = do
    (xs,e) <- unbind b
    e <- reduce e
    if null xs
        then return e
        else case e of
                App e (VarE y)
                    | take 1 xs == [VarP y]
                    && not (y `elem` fv e)
                    -> return e
                _   -> return (Abs (bind xs e))
reduce (RecE [e]) =
    reduce e
reduce (RecE es) = do
    es <- mapM reduce es
    return (RecE es)
reduce (Case bs) = do
    bs <- sequence
        [ do
            (xs,e) <- unbind b
            e <- reduce e
            return (bind xs e)
        | b <- bs
        ]
    return (Case bs)
reduce (App e1 e2) = do
    e1 <- reduce e1
    case e1 of
        Case bs -> do
            e2 <- reduce e2
            mbSubs <- matches bs e2
            case mbSubs of
                Nothing         -> return (App e1 e2)
                Just (subs,e)   -> return (substs subs e)
        _ -> do
            e2 <- reduce e2
            return (App e1 e2)
reduce other = return other

matches    []   _ = return Nothing
matches (b:bs) e2 = do
    (p,e) <- unbind b
    case match p e2 of
        Just subs   -> return (Just (subs,e))
        Nothing     -> matches bs e2

match :: Pat -> Expr -> Maybe [(Name Expr, Expr)]
match p e = loop id (recPs p) (recEs e)
    where
        recPs (RecP ps) = ps
        recPs p = [p]
        
        recEs (RecE es) = es
        recEs e = [e]
        
        loop subs [] [] = Just (subs [])
        loop subs (VarP x : ps) (e      : es)
            = loop (subs . ((x,e) :)) ps es
        loop subs (SelP x : ps) (SelE y : es)
            | x == y
            = loop subs ps es
        loop subs (x@RecP{} : ps) (y : es)
            = case match x y of
                Nothing -> Nothing
                Just subs2 -> loop (subs . (subs2 ++)) ps es
        loop subs  _  _ = Nothing


unit    = sel 0
nothing = sel 1
just    = sel 2
left    = sel 3
right   = sel 4

selP (SelE n) = SelP n

mb = lams ["f", "g"] $ cse [(selP nothing, f), (RecP [selP just, VarP (s2n "x")], app g (var "x"))]
 where
     f = var "f"
     g = var "g"

eith = lams ["f", "g"] $ cse [ (RecP [selP left,  VarP (s2n "x")], app f (var "x"))
                             , (RecP [selP right, VarP (s2n "x")], app g (var "x"))]
 where
     f = var "f"
     g = var "g"

