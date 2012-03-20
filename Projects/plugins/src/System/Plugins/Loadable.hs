{-# LANGUAGE EmptyDataDecls #-}
module System.Plugins.Loadable where

import Control.Applicative

data Loadable t

unsafeLoadWithoutPinning    :: Loadable t -> IO t

isLoaded :: Loadable t -> IO Bool

pin      :: Loadable t -> IO t
isPinned :: Loadable t -> IO Bool

unsafeLoadWithoutPinning = undefined
isLoaded    = undefined
pin         = undefined
isPinned    = undefined