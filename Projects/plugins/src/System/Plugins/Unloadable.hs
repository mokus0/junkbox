{-# LANGUAGE EmptyDataDecls #-}

-- hypothetical interface for 'unloadable' plugins.  the idea being that
-- a plugin can be loaded and then tracked at the type level so that when
-- the user asks to unload it, all values that were constructed from it
-- become invalidated.  Alternatively, the user has the option of explicitly
-- pinning the object, after which no other object derived from the same
-- module can be unloaded.
module System.Plugins.Unloadable where

import Control.Applicative
import System.Plugins.Loadable
import Control.DeepSeq

data Unloadable t

instance Functor        Unloadable
instance Applicative    Unloadable
instance Monad          Unloadable

load            :: Loadable t -> IO (Unloadable t)
unload          :: Unloadable t -> IO Bool
isUnloaded      :: Unloadable t -> IO Bool

pin             :: Unloadable t -> IO (Maybe t)
isPinned        :: Unloadable t -> IO Bool

withUnloadable  :: Unloadable a -> (a -> IO b) -> IO (Maybe (Unloadable b))

-- The idea with 'sever' is that if you have an unloadable value of a
-- type which is:
--      a) not defined in an unloadable module and which contains no values of such types
--      b) 'rnf'able
--  and
--      c) not already unloaded
-- then you can sever it from the unloadable module that produced it by
-- fully evaluating it.
-- 
class NFData t => Severable t where {- ??? -}
sever :: Severable t => Unloadable t -> IO (Maybe t)

load            = undefined
unload          = undefined
reload          = undefined
isUnloaded      = undefined
pin             = undefined
isPinned        = undefined
withUnloadable  = undefined
sever           = undefined