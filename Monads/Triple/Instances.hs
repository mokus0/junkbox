{-
 -      ``Triple/Instances.hs''
 -      (c) 2008 James Cook
 -}

module Triple.Instances where

import Triple
import Control.Monad.Identity

instance Triple [] where
        eta x           = [x]
        etaInv [x]      = Just x
        etaInv _        = Nothing
        mu              = concat

instance Triple Maybe where
        eta             = Just
        etaInv          = id
        mu              = maybe Nothing id

instance Triple Identity where
        eta             = return
        etaInv          = Just . runIdentity
        mu              = join

--
-- class (Monad m) => ReturnInv m where
--         returnInv :: m a -> Maybe a
-- 
-- instance ReturnInv Identity where
--         returnInv = Just . runIdentity
-- 
-- instance ReturnInv Maybe where
--         returnInv = id
-- 
-- instance ReturnInv ((->) a) where
--         returnInv f = Nothing
-- 
-- instance ReturnInv [] where
--         returnInv [x]   = Just x;
--         returnInv _     = Nothing
-- 
-- instance ReturnInv (State s) where
--         returnInv = const Nothing
-- 
-- instance ReturnInv (RevState s) where
--         returnInv = const Nothing
-- 
-- instance (Monad m) => ReturnInv (StateT s m) where
--         returnInv = const Nothing