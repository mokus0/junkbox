{-# LANGUAGE 
    MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, FlexibleContexts, UndecidableInstances,
    TypeFamilies,
    StandaloneDeriving,
    ViewPatterns
  #-}
module TypeSys where

import Mu
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS

data Expr_ con abs t
    = C (con t)
    | A (abs t)
    deriving (Eq, Show)

instance (Functor con, Functor abs) => Functor (Expr_ con abs) where
    fmap f (C con) = C (fmap f con)
    fmap f (A abs) = A (fmap f abs)

data ConS lit expr
    = Lit lit
    | App [expr]
    deriving (Eq, Show)

instance Functor (ConS lit) where
    fmap f (Lit l) = Lit l
    fmap f (App e) = App (map f e)

class Abstraction abs var | abs -> var where
    type Env abs :: * -> *
    type FV abs
    extendEnv :: Env abs expr -> abs expr -> Env abs expr
    readEnv :: Env abs expr -> var -> expr
    fv :: Mu mu => mu abs -> FV abs
    
data AbsS var expr
    = Var var
    | Let (M.Map var expr) expr
    deriving (Eq, Show)
instance Functor (AbsS var) where
    fmap f (Let binds expr) = Let (fmap f binds) (f expr)
    fmap f (Var x) = Var x

instance Ord var => Abstraction (AbsS var) var where
    type Env (AbsS var) = M.Map var
    type FV (AbsS var) = S.Set var
    extendEnv env (Var _) = env
    extendEnv env (Let ext _) = M.unionWith shadow env ext
        where shadow = flip const
    readEnv = (M.!)
    fv (unMu -> Var x) = S.singleton x
    fv (unMu -> Let binds expr) = S.unions $
        (fv expr) :
        [ fv lhs
        | lhs <- M.elems binds
        ]

data DeBruijnAbsS expr
    = VarI Int
    | LetI expr
    deriving (Eq, Show)
instance Functor DeBruijnAbsS where
    fmap f (VarI i) = VarI i
    fmap f (LetI e) = LetI (f e)

instance Abstraction DeBruijnAbsS Int where
    type Env DeBruijnAbsS = []
    type FV DeBruijnAbsS = IS.IntSet
    extendEnv env (VarI _) = env
    extendEnv env (LetI expr) = expr : env
    readEnv = (!!)
    fv (unMu -> VarI i) = IS.singleton i
    fv (unMu -> LetI e) = IS.map pred (fv e)

type Expr lit var = MuT (Expr_ (ConS lit) (AbsS var))
newtype AbsView con abs = AbsView (MuT (Expr_ con abs))
newtype ConView abs con = ConView (MuT (Expr_ con abs))

instance Functor con => Mu (AbsView con) where
    mu abs__AbsView_abs = absView_abs
        where
            absView_abs = undefined -- AbsView (Mu (A (fmap (foldMu mu) abs__AbsView_abs)))
    unMu absView_t = t__AbsView_t
        where
            t__AbsView_t = undefined
    {- ??? -}
instance Mu (ConView abs) where
    {- ??? -}
