{-# LANGUAGE 
        GADTs, 
        NoMonomorphismRestriction, 
        DeriveDataTypeable,
        RankNTypes
  #-}
module Susie.Module.Preferences
    ( preferences
    ) where

import Susie.Env (declare)
import Susie.Module

import Data.Dependent.Sum
import Data.GADT.Compare
import Data.PropertyList
import Data.Typeable

data PrefsKey a where
    Prefs :: PrefsKey Preferences
    deriving Typeable

instance GCompare PrefsKey where
    gcompare Prefs Prefs = GEQ

prefs = declare Prefs

data Preferences = Preferences
    { getPreference :: forall m a. Monad m => [String] -> m a
    , setPreference :: forall m a. Monad m => [String] -> a -> m ()
    }

preferences :: Monad m => m (Module m s env)
preferences = return newModule
    { name = "preferences"
--    , dependencies = [ locateResource ]
    , staticExports =
        [ prefs :=> Preferences
            { getPreference = \keyPath -> do
                return (error "GetPref: not implemented!")
            , setPreference = undefined
            }
        ]
    }