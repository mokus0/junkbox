{-# LANGUAGE 
        ExistentialQuantification,
        FlexibleContexts
  #-}
{-
 -      ``Data/PriorityQueue.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -
 -      this kicks ass, if I do say so myself ;-)
 -      the |DefaultStateRef| class makes the choice of |StateRef| instance
 -      decidable, and the laxity of the |StateRef| class's fundeps makes
 -      queues constructible in monads other than where they are intended
 -      to be used; eg:
 -      
 -         q <- mkQueue even :: IO (PriorityQueue STM Integer)
 -      
 -      after which the whole interface to the queue is:
 -         insert (x :: Integer) q :: STM ()
 -         dequeue q :: STM Integer
 -}

module Data.PriorityQueue (
                PriorityQueue,
                
                mkQueue, mkQueueBy,
                
                insert, dequeue, peek
        ) where

import Data.StateRef
import Util.ReOrd
import qualified Data.Map as M

data PQ a = forall p. Ord p =>
        PQ { priorityFunc       :: a -> p
           , queue              :: M.Map p [a]
           }

data PriorityQueue m a = forall sr. DefaultStateRef sr m (PQ a) =>
        PriorityQueue (sr)

-- heh... I love type inference - it would've taken me a long time to
-- come up with this manually
mkQueue :: (DefaultStateRef sr m1 (PQ a),
            StateRef sr m (PQ a),
            Ord p) =>
           (a -> p) -> m (PriorityQueue m1 a)
mkQueue f = do
        pq <- newRef (PQ f M.empty)
        return (PriorityQueue pq)

-- This one takes a comparator instead of a function to convert to
-- a separate priority type.
mkQueueBy :: (DefaultStateRef sr m1 (PQ a),
              StateRef sr m (PQ a)) =>
             (a -> a -> Ordering) -> m (PriorityQueue m1 a)
mkQueueBy cmp = mkQueue (ReOrd cmp)

insert :: a -> PriorityQueue m a -> m ()
insert x (PriorityQueue pqRef) = do
        modifyRef pqRef (\(PQ f pq) -> PQ f (M.insertWith (flip (++)) (f x) [x] pq))
        return ()

dequeue :: PriorityQueue m a -> m (Maybe a)
dequeue q@(PriorityQueue pqRef) = do
        PQ f pq <- readRef pqRef
        let view = M.minViewWithKey pq
        case view of
                Nothing                 -> return Nothing
                Just ((k,[]), pq')      -> do
                        -- this should never happen
                        writeRef pqRef (PQ f pq')
                        dequeue q
                Just ((k,[i]), pq')     -> do
                        writeRef pqRef (PQ f pq')
                        return (Just i)
                Just ((k,i:is), pq')    -> do
                        writeRef pqRef (PQ f (M.insert k is pq'))
                        return (Just i)

peek :: PriorityQueue m a -> m [a]
peek (PriorityQueue pqRef) = do
        PQ f pq <- readRef pqRef
        let (ks, vss) = unzip (M.toAscList pq)
        return (concat vss)
        