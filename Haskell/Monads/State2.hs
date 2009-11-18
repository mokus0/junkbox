{-
 -      ``State2.hs''
 -      (c) 2008 James Cook
 -}

module State2 where

import Control.Monad.State (MonadState(..))
import Triple (Triple(..))

data State    s a = State    (s -> (s,a))
                  | S_Eta    a
data RevState s a = RevState (s -> (s,a))
                  | RS_Eta   a

foldState f1 f2 (State s) = f1 s
foldState f1 f2 (S_Eta x) = f2 x

foldRevState f1 f2 (RevState s) = f1 s
foldRevState f1 f2 (RS_Eta   x) = f2 x

runState    = foldState    id (flip (,))
runRevState = foldRevState id (flip (,))

apSnd f (a, b) = (a, f b)

instance Functor (State s) where
        fmap f (State s) = State (apSnd f . s)
instance Functor (RevState s) where
        fmap f (RevState s) = RevState (apSnd f . s)

joinFw, joinRev :: (s -> (s, s -> (s, a))) -> s -> (s, a)
joinFw f s = (s'', c)
        where
                (s', g) = f s
                (s'', c) = g s'

joinRev f s = (s'', c)
        where
                (s'', g) = f s'
                (s',  c) = g s

--joinState :: (State s (State s a)) -> State s a
--joinState (S_Eta s2)   = s2
--joinStete (State s1) = State composed
--    where
--        composed s = (s'', c)
--            where
--                (s',  s2)   = s1 s'
--                (s'',  c)   = runState s2 s
--
--joinRevState :: (RevState s (RevState s a)) -> RevState s a
--joinRevState (RS_Eta r2)     = r2
--joinRevState (RevState r1) = RevState composed
--        where
--            composed s = (s'', c)
--                where
--                    (s'', r2)  = r1 s'
--                    (s',  c)   = runRevState r2 s

instance Triple (State s) where
        eta = S_Eta
        etaInv = foldState (const Nothing) Just
        mu = foldState (State . joinFw . fmap (fmap runState)) id

instance Triple (RevState s) where
        eta = RS_Eta
        etaInv = foldRevState (const Nothing) Just
        mu = foldRevState (RevState . joinRev . fmap (fmap runRevState)) id

instance MonadState s (State s) where
        get = State (\s -> (s, s))
        put = \s -> State (\_ -> (s, ()))

instance MonadState s (RevState s) where
        get = RevState (\s -> (s, s))
        put = \s -> RevState (\_ -> (s, ()))

