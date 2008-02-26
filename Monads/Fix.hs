{-# OPTIONS -fglasgow-exts #-}
{-
 -      ``/Users/mokus/Projects/mokus-sandbox/Monads/Fix.hs''
 -      (c) 2008 James Cook
 -}

module Fix where

import Triple hiding (Var, fold)

data Fix m a 
        = Var a
        | Fix (m (Fix m a))

fix m = case etaInv m of
        Just a  -> Var a
        Nothing -> Fix (fmap eta m)

fold f1 f2 (Var a) = f1 a
fold f1 f2 (Fix m) = f2 (fmap (fold f1 f2) m)

instance (Functor m) => Functor (Fix m) where
        fmap f (Var a) = Var (f a)
        fmap f (Fix m) = Fix (fmap (fmap f) m)

simplify m = case etaInv m >>= etaInv of
        Nothing         -> Fix m
        Just x          -> Var x

instance (Triple m) => Triple (Fix m) where
        eta             = Var
        
        etaInv = fold Just (mu.etaInv)
        
        mu = fold id simplify
        
instance Show a => Show (Fix [] a) where
        showsPrec p (Var a) = showParen (p > 10) (showString "Var " . showsPrec 0 a)
        showsPrec p (Fix m) = showParen (p > 10) (showString "Fix " . showsPrec 0 m)

instance Show a => Show (Fix Maybe a) where
        showsPrec p (Var a) = showParen (p > 10) (showString "Var " . showsPrec 0 a)
        showsPrec p (Fix m) = showParen (p > 10) (showString "Fix " . showsPrec 0 m)

flatten (Var a) = return a
flatten (Fix m) = mu (fmap flatten m)