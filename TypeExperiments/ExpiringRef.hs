{-# LANGUAGE RecursiveDo #-}
module TypeExperiments.ExpiringRef where

import System.Mem.Weak
import Control.Concurrent

-- | Expiring reference: created with a time limit.  Access within that time
-- will succeed, accesses after that may or may not succeed.  
newtype Expiring a = Expiring (Weak a)

-- |Create an expiring reference, with lifetime the given number of seconds.
-- Reference will expire even if the value itself is kept alive.
newExpiringRef val time = mdo
    key <- key `keepAliveSeconds` time
    
    wRef <- mkWeak key val Nothing
    return (Expiring wRef)

-- |Create an expiring reference, with lifetime the given number of seconds.
-- Reference will not expire if the value itself is kept alive.
-- (note that in GHCi the value will be kept alive if it was ever
-- bound to a name, even if now shadowed - this includes the name "it";
-- to check the expired-ness without causing it to be held forever, use 
-- something like "readExpiring x >>= print" so that it doesn't get bound
-- to "it")
newExpiringRef' val time = do
    val `keepAliveSeconds` time
    
    wRef <- mkWeakPtr val Nothing
    return (Expiring wRef)

readExpiring (Expiring wRef) = deRefWeak wRef

-- |Keep a value alive (prevent it from being released
-- by the garbage collector) for a specified number of seconds.
keepAliveSeconds val sec = mdo
    tid <- forkIO $ do
        threadDelay (round (sec * 1e6))
        val `seq` return () -- keep value alive till here
        tid `seq` return () -- keep self alive too!
    return tid