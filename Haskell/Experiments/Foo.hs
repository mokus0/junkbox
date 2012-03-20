{-# LANGUAGE GADTs #-}
module Experiments.Foo where

import Control.Applicative
import Data.Dependent.Sum
import qualified Data.Dependent.Map as M
import qualified Data.Set as S

data Module ident t where
    Module :: t -> Module ident t
    Dep :: ident a -> Module ident (a -> b) -> Module ident b
    
loadModule :: M.GCompare ident => M.DMap ident -> Module ident t -> Maybe t
loadModule _ (Module m) = Just m
loadModule e (Dep ident m) = loadModule e m <*> M.lookup ident e

data Binding ident where
    Binding :: ident t -> Module ident t -> Binding ident