{-# LANGUAGE 
        ExistentialQuantification,
        FlexibleContexts
  #-}
{-
 -      ``Data/PriorityQueue.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -
 -      this kicks ass, if I do say so myself ;-)
 -      the |DefaultStateRef| class makes this highly usable, and the
 -      laxity of the |StateRef| class's fundeps makes queues constructible
 -      in monads other than where they are intended to be used; eg:
 -      
 -         q <- mkQueue even :: IO (PriorityQueue STM Integer)
 -      
 -      after which the whole interface to the queue is:
 -         insert (x :: Integer) q :: STM ()
 -         dequeue q :: STM Integer
 -}

module Data.PriorityQueue where

import Data.StateRef
import qualified Data.Map as M

data PQ a = forall p. Ord p =>
        PQ { priorityFunc       :: a -> p
           , queue              :: M.Map p [a]
           }

data PriorityQueue m a = forall sr. DefaultStateRef sr m (PQ a) =>
        PriorityQueue (sr)

-- mkQueue :: (a -> p) -> m1 (PriorityQueue m a)
mkQueue f = do
        pq <- newRef (PQ f M.empty)
        return (PriorityQueue pq)

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
                Just ((k,[]), pq')          -> do
                        writeRef pqRef (PQ f pq')
                        dequeue q
                Just ((k,i:is), pq')        -> do
                        writeRef pqRef (PQ f (M.insert k is pq'))
                        return (Just i)