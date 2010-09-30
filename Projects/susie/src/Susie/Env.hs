{-# LANGUAGE
        GADTs
  #-}
module Susie.Env
    ( module Susie.Env.Var
    
    , Env
    , Entry(..)
    , Binding(..)
    
    , empty
    , setEnv, getEnv
    , bindings
    ) where

import Data.Dependent.Sum
import Data.GADT.Compare
import Susie.Env.Var
import Susie.Module.ModuleID
import qualified Data.Dependent.Map as M

-- An environment is a mapping from variables to values
newtype Env s = Env 
    { entriesByVar  :: M.DMap (LiftVar Entry s)
    }

data LiftVar f s a where
    LiftVar :: !(Var s a) -> LiftVar f s (f s a)

instance GCompare (LiftVar f s) where
    gcompare (LiftVar v1) (LiftVar v2) = case gcompare v1 v2 of
        GLT -> GLT; GEQ -> GEQ; GGT -> GGT

data Entry s a = Entry
    { provider  :: ModuleID s
    , value     :: a
    }

data Binding s where
    Binding ::
        { bindingVar      :: Var s a
        , bindingProvider :: ModuleID s
        , bindingValue    :: a
        } -> Binding s

empty :: Env s
empty = Env M.empty

setEnv :: Var s a -> Entry s a -> Env s -> Env s
setEnv var entry (Env env) = Env (M.insert (LiftVar var) entry env)

getEnv :: Var s a -> Env s -> Maybe (Entry s a)
getEnv var (Env env) = M.lookup (LiftVar var) env

bindings :: Env s -> [Binding s]
bindings (Env env) = [Binding var modId val | LiftVar var :=> Entry modId val <- M.toList env]

