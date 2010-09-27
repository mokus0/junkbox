{-# LANGUAGE RankNTypes, GADTs, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module TypeExperiments.Iteratee where

import Prelude hiding (head, drop)
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Operational

class Iteratee it where
    enum :: it el a -> [el] -> it el a
    run  :: it el a -> Maybe a

class Iteratee it => Examples it where
    head :: it el (Maybe el)
    peek :: it el (Maybe el)
    drop :: Int -> it el ()

-- non-monadic iteratee type from John Lato's Monad Reader 16 article:
data StreamG el = Empty | El el | EOF
data IterV el a 
    = Done a (StreamG el)
    | Cont (StreamG el -> IterV el a)

instance Monad (IterV el) where
    return x = Done x Empty
    m >>= f = case m of
        Done x str -> case f x of
            Done x' _ -> Done x' str
            Cont k	-> k str
        Cont k -> Cont (\str -> k str >>= f)

instance Functor (IterV el) where
    fmap f (Done x str) = Done (f x) str
    fmap f (Cont k) = Cont (fmap f . k)

instance Applicative (IterV el) where
    pure x = Done x Empty
    (Done f str) <*> i2 = fmap f i2
    (Cont k) <*> i2 = Cont (\str -> k str <*> i2)

instance Iteratee IterV where
    enum i [] = i
    enum i@(Done _ _) _ = i
    enum (Cont k) (x:xs) = enum (k (El x)) xs

    run (Done x _) = Just x
    run (Cont k) = run' (k EOF)
        where
            run' (Done x _) = Just x
            run' _ = Nothing

instance Examples IterV where
    head = Cont step
        where
            step (El el) = Done (Just el) Empty
            step Empty = Cont step
            step EOF = Done Nothing EOF
    peek = Cont step
        where
            step c@(El el) = Done (Just el) c
            step Empty = Cont step
            step EOF = Done Nothing EOF
    drop 0 = Done () Empty
    drop n = Cont step
        where
            step (El _) = drop (n-1)
            step Empty = Cont step
            step EOF = Done () EOF

-- proposed new iteratee type:
-- (conceptually more modular, and fairly easy to imagine interesting extensions)
-- Exported: Iter, getSym, unGetSym, enum, run
--
-- Basically, 'Cont' in the 'IterV' type corresponds exactly to a "prompt" in
-- Control.Monad.Prompt parlance.  There is only one prompt constructor, "GetSym".
-- There is also a threaded state that is used to store fetched-but-unconsumed input.
-- This scheme could easily be modified to support more lookahead, or to extend
-- a base monad, yielding an Iteratee monad transformer.  Or lookahead management
-- could be shifted entirely out of the monad and into the enumerator primitives
-- (eg, add a Peek or UnGet constructor to the GetSym type and drop the State
-- component of the Iter type).
-- 
-- TODO: define some useful equivalence (~=) and attempt to prove Iter ~= IterV.  
-- (I think there is not quite a type isomorphism, because in the Done state there
-- is no "state" component, but there may be some well-defined set of circumstances
-- under which the state can be considered irrelevant, such that up to state in
-- those circumstances the types are isomorphic.  That same near-isomorphism should
-- also be a monad (near-?)isomorphism.  The 'head', 'peek', and 'drop' example 
-- functions as given should be images of each other under that transformation)
-- 
-- TODO: generalize to monadic iteratees and attempt to prove a useful equivalence.
--
-- Miscellaneous observations:  The essence of the "iteratee" concept seems to
-- be captured by Control.Monad.Operational.'viewT'.  It is simple:  The computation
-- can be evaluated to a sort of a "monadic WHNF": All effects applied up until
-- the program is ready to either return a value or needs more information.  That
-- abstraction is really entirely independent of the nature of the information 
-- needed;  the "Iteratee" concept is an instance of the "ProgramT" or 
-- "MonadPrompt" concept, and an "Enumerator" is just a function that
-- transforms a "Program" by answering as many of its requests as it can, then
-- stopping (leaving the program in whatever state it may be in at that point).
data GetSym el a where GetSym :: GetSym el (Maybe el)
newtype Iter el a = Iter { unIter :: ProgramT (GetSym el) (State (StreamG el)) a }
    deriving (Functor, Monad)
instance Applicative (Iter el) where
    pure = return
    (<*>) = ap

requireSym :: Iter el el
requireSym = Iter $ do
    str <- lift get
    case str of
        El el -> do
            lift (put Empty)
            return el
        _ -> do
            mbSym <- singleton GetSym
            maybe (unIter requireSym) return mbSym

-- Gets a symbol from the input, returning Nothing on EOF.
requestSym :: Iter el (Maybe el)
requestSym = Iter $ do
    str <- lift get
    case str of
        El el -> do
            lift (put Empty)
            return (Just el)
        Empty -> singleton GetSym
        EOF -> return Nothing

peekSym :: Iter el (Maybe el)
peekSym = Iter $ do
    str <- lift get
    case str of
        El el -> do
            return (Just el)
        Empty -> do
            mbSym <- singleton GetSym
            lift (put (maybe EOF El mbSym))
            return mbSym
        EOF -> return Nothing

instance Iteratee Iter where
    enum i [] = i
    enum (Iter i) (x:xs) = case runState (viewT i) Empty of
        (Return x,          _) -> Iter i
        (GetSym :>>= k, Empty) -> enum (Iter (k (Just x))) xs
    
    run (Iter i) = case evalState (viewT i) EOF of
        Return x -> Just x
        _        -> Nothing
        

instance Examples Iter where
    head = requestSym
    peek = peekSym
    drop 0 = return ()
    drop n = do requestSym; drop (n-1)

