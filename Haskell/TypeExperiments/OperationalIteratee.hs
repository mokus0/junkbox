{-# LANGUAGE GADTs, ViewPatterns #-}
module TypeExperiments.OperationalIteratee where

import Control.Monad.Identity
import Control.Monad.Operational

data GetSym a b where GetSym :: GetSym a (StreamG a)

type Iteratee s a = Program (GetSym s) (a, StreamG s)

-- non-monadic iteratee type from John Lato's Monad Reader 16 article:
data StreamG el = Empty | El el | EOF
data IterV el a 
    = DoneV a (StreamG el)
    | Cont (StreamG el -> IterV el a)

-- Isomorphism between @IterV el a@ and @Suspended (GetSym el) a@
-- (IMPORTANT:  THIS ISOMORPHISM IS NOT A MONAD MORPHISM!
-- I think one could be found by letting:
-- > type Iteratee s a = ProgramT (GetSym s) (State (StreamG s)) a
-- or something similar, and making higher level "getSym" and "unGetSym" 
-- operations that make appropriate use of the stash)
f :: IterV el a -> Iteratee el a
f (DoneV x rest) = return (x, rest)
f (Cont k) = singleton GetSym >>= f . k

g :: Iteratee el a -> IterV el a
g = g' . view
    where
        g' :: ProgramView (GetSym s) (a, StreamG s) -> IterV s a
        g' (Return (x,rest)) = DoneV x rest
        g' (GetSym :>>= k)   = Cont (g . k)

-- Proof:
-- 
--      f.g = id
--      --------
-- 
-- f . g' $ Return (x,rest)
-- f $ DoneV x rest
-- return (x,rest)
-- 
-- f . g' $ GetSym :>>= k
-- f $ Cont (g . k)
-- GetSym :>>= (f . g . k)
-- {-by induction-} GetSym :>>= k
-- 
--      g.f = id
--      --------
-- 
-- g . f $ DoneV x rest
-- g' $ Return (x, rest)
-- DoneV x rest
-- 
-- g . f $ Cont k
-- g' $ GetSym :>>= (f . k)
-- Cont (g . f . k)
-- {-by induction-} Cont k
