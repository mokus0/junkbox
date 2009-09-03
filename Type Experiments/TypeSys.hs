{-# LANGUAGE 
    MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, FlexibleContexts, UndecidableInstances,
    TypeFamilies,
    StandaloneDeriving,
    ViewPatterns
  #-}
module TypeSys where

import MuClass
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
    fv = S.unions . map fv' . unMu
        where
            fv' (Var x) = S.singleton x
            fv' (Let binds expr) = S.unions $
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
    fv = IS.unions . map fv' . unMu
        where
            fv' (VarI i) = IS.singleton i
            fv' (LetI e) = IS.map pred (fv e)

type Expr lit var = MuT (Expr_ (ConS lit) (AbsS var))

newtype AbsView con abs = AbsView (MuT (Expr_ con abs))
unAbsView (AbsView x) = x
deriving instance 
    ( Eq (con (MuT (Expr_ con abs)))
    , Eq (abs (MuT (Expr_ con abs)))
    ) => Eq (AbsView con abs)
deriving instance 
    ( Show (con (MuT (Expr_ con abs)))
    , Show (abs (MuT (Expr_ con abs)))
    ) => Show (AbsView con abs)

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

instance Functor con => Mu (AbsView con) where
    mu = AbsView . Mu . A . fmap unAbsView
    unMu = unA . unMuT . unAbsView
        where
            unA (A a) = [fmap AbsView a]
            unA (C c) = undefined
                    where
                        _ = (c `asTypeOf` (undefined :: con (MuT (Expr_ con abs))))
                    
             --undefined
    {- ??? -}
instance Mu (ConView abs) where
    mu = ConView . Mu . C . fmap unConView
    {- ??? -}
    
muMap2 f (Mu x) = mu (f (fmap (muMap2 f) x))
