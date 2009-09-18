{-# LANGUAGE 
    MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, FlexibleContexts, UndecidableInstances,
    TypeFamilies,
    StandaloneDeriving,
    ViewPatterns, 
    DeriveFunctor, DeriveFoldable, DeriveTraversable
  #-}
-- GHC 6.11+ required for DeriveFunctor et al
module TypeSys where

import MuClass
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Foldable as F
import qualified Data.Traversable as T

data ConS lit expr
    = Lit lit
    | App [expr]
    deriving (Eq, Show, Functor, F.Foldable, T.Traversable)

data AbsS var expr
    = Var var
    | Lam [var] expr
    deriving (Eq, Show, Functor, F.Foldable, T.Traversable)

data DeBruijnAbsS expr
    = VarI Int
    | LamI expr
    deriving (Eq, Show, Functor, F.Foldable, T.Traversable)

class HasVar expr var | expr -> var where
    type FV expr
    var :: var -> expr t
    fv :: Mu mu => mu expr -> FV expr

instance Ord var => HasVar (AbsS var) var where
    type FV (AbsS var) = S.Set var
    var = Var
    fv = foldMu (S.unions . map fv')
        where
            fv' (Var x) = S.singleton x
            fv' (Lam binds expr) = expr S.\\ S.fromList binds

instance HasVar DeBruijnAbsS Int where
    type FV DeBruijnAbsS = IS.IntSet
    var = VarI
    fv = foldMu (IS.unions . map fv')
        where
            fv' (VarI i) = IS.singleton i
            fv' (LamI e) = IS.filter (>= 0) (IS.map pred e)

data Expr_ con abs t
    = C (con t)
    | A (abs t)
    deriving (Eq, Show, Functor, F.Foldable, T.Traversable)

type Expr lit var = MuT (Expr_ (ConS lit) (AbsS var))
type DeBruijnExpr lit var = MuT (Expr_ (ConS lit) DeBruijnAbsS)

newtype AbsView mu con abs = AbsView (mu (Expr_ con abs))
unAbsView (AbsView x) = x
deriving instance 
    ( Eq (mu (Expr_ con abs))
    ) => Eq (AbsView mu con abs)
deriving instance 
    ( Show (mu (Expr_ con abs))
    ) => Show (AbsView mu con abs)

newtype ConView abs con = ConView (MuT (Expr_ con abs))
unConView (ConView x) = x
deriving instance 
    ( Eq (con (MuT (Expr_ con abs)))
    , Eq (abs (MuT (Expr_ con abs)))
    ) => Eq (ConView abs con)
deriving instance 
    ( Show (con (MuT (Expr_ con abs)))
    , Show (abs (MuT (Expr_ con abs)))
    ) => Show (ConView abs con)

instance (Mu mu, Functor con, F.Foldable con) => Mu (AbsView mu con) where
    mu = AbsView . mu . A . fmap unAbsView
    unMu = (>>= unA) . unMu . unAbsView
        where
            unA (A a) = return (fmap AbsView a)
            unA (C c) = F.toList c >>= (unMu . AbsView)
    {- ??? -}
instance F.Foldable abs => Mu (ConView abs) where
    mu = ConView . Mu . C . fmap unConView
    unMu = unC . unMuT . unConView
        where
            unC (A a) = F.toList a >>= (unMu . ConView)
            unC (C c) = return (fmap ConView c)
    {- ??? -}
    
instance (F.Foldable con, Functor con, HasVar abs var) => HasVar (Expr_ con abs) var where
    type FV (Expr_ con abs) = FV abs
    var = A . var
    fv expr = fv (AbsView expr)

