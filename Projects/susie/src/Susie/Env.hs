{-# LANGUAGE
        GADTs, 
        RankNTypes,
        FlexibleInstances
  #-}
module Susie.Env
    ( module Susie.Env.Var
    , module Susie.Env.Var.TH
    
    , Env
    , Entry(..)
    , Binding(..)
    
    , empty
    , setEnv, getEnv
    , filterEnv
    , bindings
    , flattenEnvs
    ) where

import Data.Dependent.Sum
import Data.GADT.Compare
import Susie.Env.Var
import Susie.Env.Var.TH
import Susie.ModuleID
import qualified Data.Dependent.Map as M

-- An environment is a mapping from variables to values
newtype Env s = Env (M.DMap (Subst1 (Entry s) (Var s)))

data Subst1 f x a where
    Subst1 :: !(x a) -> Subst1 f x (f a)

instance GEq x => GEq (Subst1 f x) where
    geq (Subst1 v1) (Subst1 v2) = case geq v1 v2 of
        Just Refl -> Just Refl; Nothing -> Nothing

instance GCompare x => GCompare (Subst1 f x) where
    gcompare (Subst1 v1) (Subst1 v2) = case gcompare v1 v2 of
        GLT -> GLT; GEQ -> GEQ; GGT -> GGT

data Entry s a = Entry
    { provider  :: !ModuleID
    , value     :: !a
    } deriving (Eq, Ord, Read, Show)

data Binding s where
    Binding ::
        { bindingVar      :: Var s a
        , bindingProvider :: ModuleID
        , bindingValue    :: a
        } -> Binding s

empty :: Env s
empty = Env M.empty

setEnv :: Var s a -> Entry s a -> Env s -> Env s
setEnv var entry (Env env) = Env (M.insert (Subst1 var) entry env)

getEnv :: Var s a -> Env s -> Maybe (Entry s a)
getEnv var (Env env) = M.lookup (Subst1 var) env

filterEnv :: (forall a. Var s a -> Entry s a -> Bool) -> Env s -> Env s
filterEnv f (Env e) = Env (M.filterWithKey (\(Subst1 k) v -> f k v) e)

bindings :: Env s -> [Binding s]
bindings (Env env) = [Binding var modId val | Subst1 var :=> Entry modId val <- M.toList env]

flattenEnvs :: [Env s] -> Env s
flattenEnvs envs = Env $ M.unions [env | Env env <- envs]