{-# LANGUAGE
        GADTs,
        GeneralizedNewtypeDeriving,
        MultiParamTypeClasses
  #-}
module TypeExperiments.Iteratee2 where

import Control.Monad.Either
import Control.Monad.State
import Control.Monad.Operational
import Data.Enumerator (Iteratee(..), Step(..), Stream(..))

-- From Data.Enumerator (enumerator-0.1.1):
-- newtype Iteratee e a m b
--   = Iteratee {runIteratee :: m (Step e a m b)}
-- data Step e a m b
--   = Continue (Stream a -> Iteratee e a m b)
--   | Yield b (Stream a)
--   | Error e
-- data Stream a = Chunks [a] | EOF

data IterP s a where
    Get :: IterP s (Stream s)

newtype Iter e a m b = Iter { runIter :: ProgramT (IterP a) (StateT (Stream a) (EitherT e m)) b }
    deriving Functor

instance Monad m => Monad (Iter e a m) where
    return = Iter . return
    fail   = lift . fail
    Iter x >>= f = Iter (x >>= runIter . f)

instance MonadTrans (Iter e a) where
    lift = Iter . lift . lift . liftE

run :: Monad m => Iter e a m b -> m (Either e b)
run (Iter p) = runEitherT (evalStateT (runIt p) EOF)
    where
        runIt :: Monad m => ProgramT (IterP a) (StateT (Stream a) (EitherT e m)) b -> StateT (Stream a) (EitherT e m) b
        runIt p = do
            v <- viewT p
            case v of
                Return x -> return x
                Get :>>= k -> runIt (k EOF)

-- |Not for export; use getStream and ungetStream instead.
get' :: Monad m => Iter e a m (Stream a)
get' = Iter $ lift $ get
put' :: Monad m => Stream a -> Iter e a m ()
put' = Iter . lift . put

-- |EitherT doesn't have a MonadTrans instance.  I don't want to make an
-- orphan, so here's its lift operation:
liftE :: Monad m => m a -> EitherT b m a
liftE x = EitherT (liftM Right x)

-- This is the approved equivalent of returning "Continue".
-- You could just invoke 'singleton Get' directly, but this
-- version guarantees sure you're not ignoring any saved input.
getStream :: Monad m => Iter e a m (Stream a)
getStream = do
    st <- get'
    case st of
        Chunks (_:_)    -> put' (Chunks []) >> return st
        _               -> Iter (singleton Get)

-- This is how 'Iter' computations put back unused input.
ungetStream :: Monad m => Stream a -> Iter e a m ()
ungetStream (Chunks xs@(_:_)) = do
    current <- get'
    case current of
        (Chunks ys@(_:_))   -> put' (Chunks (xs ++ ys))
        _                   -> put' (Chunks xs)
ungetStream _ = return ()

-- This is how 'Iter' computations report errors.  Due to the stacking order of the
-- monads (which was chosen to make Iter and Iteratee as close to isomorphic as possible),
-- any unused input is lost.  Thus, there can never really be a 'catch', just as there
-- really couldn't be a sensible 'catch' in the 'Iteratee' type due to the absence
-- of any 'Stream a' component on the 'Error' data constructor.
throwIter :: Monad m => e -> Iter e a m b
throwIter = Iter . lift . lift . EitherT . return . Left

type IterView e a m b = (ProgramViewT (IterP a) (StateT (Stream a) (EitherT e m)) b, Stream a)

viewToStep :: Monad m => IterView e a m b -> Step e a m b
viewToStep (Return x,   s)  = Yield x s
viewToStep (Get :>>= k, _)  = Continue (iterToIteratee . Iter . k)

iterToIteratee :: Monad m => Iter e a m b -> Iteratee e a m b
iterToIteratee (Iter p) = Iteratee $ do
    v <- runEitherT (runStateT (viewT p) (Chunks []))
    either (return . Error) (return . viewToStep) v

iterateeToIter :: Monad m => Iteratee e a m b -> Iter e a m b
iterateeToIter (Iteratee i) = do
    step <- lift i
    case step of
        Continue k -> do
            s <- getStream
            iterateeToIter (k s)
        Yield x s -> do
            ungetStream s
            return x
        Error e -> throwIter e

-- Hypothesis 1 :: iterateeToIter . iterToIteratee == id
-- (Up to structure representable by "approved" primitives)
{-
    --- return ---
    \x -> iterateeToIter (iterToIteratee (return x))
    \x -> iterateeToIter (Iteratee (return (viewToStep (Return x, Chunks []))))
    \x -> iterateeToIter (Iteratee (return (Yield x (Chunks []))))
    \x -> ungetStream (Chunks []) >> return x
    \x -> return x
    
    --- fmap ---
    iterateeToIter (iterToIteratee (fmap f x))
    ...
    fmap f x
    (assumed to hold, for now)
    
    --- (>>=) ---
    iterateeToIter (iterToIteratee (x >>= f))
    ...
    x >>= f
    (assumed to hold, for now)
    
    --- fail ---
    (doesn't hold, because Iteratee uses "fail = error" while Iter passes fail up the chain to 'm')
    
    --- lift ---
    iterateeToIter (iterToIteratee (lift x))
    ...
    lift x
    (assumed to hold, for now)
    
    --- getStream ---
    iterateeToIter (iterToIteratee getStream)
    iterateeToIter (iterToIteratee (get' >>= \st -> case st of Chunks (_:_) -> put' (Chunks []) >> return st; _ -> Iter (singleton Get)))
    iterateeToIter (Iteratee (runEitherT (runStateT (viewT (runIter (get' >>= \st -> case st of Chunks (_:_) -> put' (Chunks []) >> return st; _ -> Iter (singleton Get)))) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
    iterateeToIter (Iteratee (runEitherT (runStateT (viewT (runIter get' >>= \st -> runIter (case st of Chunks (_:_) -> put' (Chunks []) >> return st; _ -> Iter (singleton Get)))) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
    iterateeToIter (Iteratee (runEitherT (runStateT (viewT (lift get >>= \st -> case st of Chunks (_:_) -> lift (put (Chunks [])) >> return st; _ -> singleton Get)) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
    iterateeToIter (Iteratee (runEitherT (runStateT (get >>= \st -> viewT (case st of Chunks (_:_) -> lift (put (Chunks [])) >> return st; _ -> singleton Get)) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
    iterateeToIter (Iteratee (runEitherT (runStateT (get >>= \st -> case st of Chunks (_:_) -> viewT (lift (put (Chunks [])) >> return st); _ -> viewT (singleton Get)) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
    iterateeToIter (Iteratee (runEitherT (runStateT (get >>= \st -> case st of Chunks (_:_) -> put (Chunks []) >> return (Return st); _ -> return (Get :>>= return)) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
    iterateeToIter (Iteratee (runEitherT (case Chunks [] of Chunks (_:_) -> runStateT (put (Chunks []) >> return (Return (Chunks []))) (Chunks []); _ -> runStateT (return (Get :>>= return)) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
    iterateeToIter (Iteratee (runEitherT (runStateT (return (Get :>>= return)) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
    iterateeToIter (Iteratee (runEitherT (return (Get :>>= return, Chunks [])) >>= either (return . Error) (return . viewToStep)))
    iterateeToIter (Iteratee (return (Right (Get :>>= return, Chunks [])) >>= either (return . Error) (return . viewToStep)))
    iterateeToIter (Iteratee (either (return . Error) (return . viewToStep) (Right (Get :>>= return, Chunks []))))
    iterateeToIter (Iteratee ((return . viewToStep) ((Get :>>= return, Chunks []))))
    iterateeToIter (Iteratee (return (viewToStep (Get :>>= return, Chunks []))))
    iterateeToIter (Iteratee (return (Continue (iterToIteratee . Iter . return))))
    lift (return (Continue (iterToIteratee . Iter . return))) >>= \step -> case step of Continue k -> getStream >>= iterateeToIter . k; Yield x s -> ungetStream s >> return x; Error e -> throwIter e
    return (Continue (iterToIteratee . Iter . return)) >>= \step -> case step of Continue k -> getStream >>= iterateeToIter . k; Yield x s -> ungetStream s >> return x; Error e -> throwIter e
    case Continue (iterToIteratee . Iter . return) of Continue k -> getStream >>= iterateeToIter . k; Yield x s -> ungetStream s >> return x; Error e -> throwIter e
    getStream >>= iterateeToIter . iterToIteratee . Iter . return
    getStream >>= Iter . return     {- by "return" case above -}
    getStream
    
    --- ungetStream ---
    \s -> iterateeToIter (iterToIteratee (ungetStream s))
    \s -> iterateeToIter (iterToIteratee (case s of {Chunks xs@(_:_) -> get' >>= \current -> case current of {Chunks ys@(_:_) -> put' (Chunks (xs ++ ys)); _ -> put' (Chunks xs)}; _ -> return ()}))
        --- s = EOF ---
        \s@EOF -> iterateeToIter (iterToIteratee (case EOF of {Chunks xs@(_:_) -> get' >>= \current -> case current of {Chunks ys@(_:_) -> put' (Chunks (xs ++ ys)); _ -> put' (Chunks xs)}; _ -> return ()}))
        \s@EOF -> iterateeToIter (iterToIteratee (return ()))
        \s@EOF -> return ()
        \s@EOF -> ungetStream s
        --- s = Chunks [] ---
        \s@(Chunks []) -> iterateeToIter (iterToIteratee (case Chunks [] of {Chunks xs@(_:_) -> get' >>= \current -> case current of {Chunks ys@(_:_) -> put' (Chunks (xs ++ ys)); _ -> put' (Chunks xs)}; _ -> return ()}))
        \s@(Chunks []) -> iterateeToIter (iterToIteratee (return ()))
        \s@(Chunks []) -> return ()
        \s@(Chunks []) -> ungetStream s
        --- s = Chunks (c:cs) ---
        \s@(Chunks (c:cs)) -> iterateeToIter (iterToIteratee (case Chunks (c:cs) of {Chunks xs@(_:_) -> get' >>= \current -> case current of {Chunks ys@(_:_) -> put' (Chunks (xs ++ ys)); _ -> put' (Chunks xs)}; _ -> return ()}))
        \s@(Chunks (c:cs)) -> iterateeToIter (iterToIteratee (let xs = c:cs in get' >>= \current -> case current of {Chunks ys@(_:_) -> put' (Chunks (xs ++ ys)); _ -> put' (Chunks xs)}))
        \s@(Chunks (c:cs)) -> iterateeToIter (iterToIteratee (get' >>= \current -> case current of {Chunks ys@(_:_) -> put' (Chunks (c:cs ++ ys)); _ -> put' (Chunks (c:cs))}))
        \s@(Chunks (c:cs)) -> iterateeToIter (Iteratee (runEitherT (runStateT (viewT (runIter (get' >>= \current -> case current of {Chunks ys@(_:_) -> put' (Chunks (c:cs ++ ys)); _ -> put' (Chunks (c:cs))}))) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
        \s@(Chunks (c:cs)) -> iterateeToIter (Iteratee (runEitherT (runStateT (viewT (lift get >>= \current -> case current of {Chunks ys@(_:_) -> runIter (put' (Chunks (c:cs ++ ys))); _ -> runIter (put' (Chunks (c:cs)))})) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
        \s@(Chunks (c:cs)) -> iterateeToIter (Iteratee (runEitherT (runStateT (get >>= \current -> case current of {Chunks ys@(_:_) -> viewT (runIter (put' (Chunks (c:cs ++ ys)))); _ -> viewT (runIter (put' (Chunks (c:cs))))}) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
        \s@(Chunks (c:cs)) -> iterateeToIter (Iteratee (runEitherT (case Chunks [] of {Chunks ys@(_:_) -> runStateT (viewT (runIter (put' (Chunks (c:cs ++ ys))))) (Chunks []); _ -> runStateT (viewT (runIter (put' (Chunks (c:cs))))) (Chunks [])}) >>= either (return . Error) (return . viewToStep)))
        \s@(Chunks (c:cs)) -> iterateeToIter (Iteratee (runEitherT (runStateT (viewT (runIter (put' (Chunks (c:cs))))) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
        \s@(Chunks (c:cs)) -> iterateeToIter (Iteratee (runEitherT (runStateT (viewT (lift (put (Chunks (c:cs))))) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
        \s@(Chunks (c:cs)) -> iterateeToIter (Iteratee (runEitherT (runStateT (put (Chunks (c:cs)) >> return (Return ())) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
        \s@(Chunks (c:cs)) -> iterateeToIter (Iteratee (runEitherT (return (Return (), Chunks (c:cs))) >>= either (return . Error) (return . viewToStep)))
        \s@(Chunks (c:cs)) -> iterateeToIter (Iteratee (return (Right (Return (), Chunks (c:cs))) >>= either (return . Error) (return . viewToStep)))
        \s@(Chunks (c:cs)) -> iterateeToIter (Iteratee ((return . viewToStep) (Return (), Chunks (c:cs))))
        \s@(Chunks (c:cs)) -> iterateeToIter (Iteratee ((return . viewToStep) (Return (), Chunks (c:cs))))
        \s@(Chunks (c:cs)) -> iterateeToIter (Iteratee (return (Yield () (Chunks (c:cs)))))
        \s@(Chunks (c:cs)) -> lift (return (Yield () (Chunks (c:cs)))) >>= \step -> case step of Continue k -> getStream >>= iterateeToIter . k; Yield x s -> ungetStream s >> return x; Error e -> throwIter e
        \s@(Chunks (c:cs)) -> case Yield () (Chunks (c:cs)) of Continue k -> getStream >>= iterateeToIter . k; Yield x s -> ungetStream s >> return x; Error e -> throwIter e
        \s@(Chunks (c:cs)) -> ungetStream (Chunks (c:cs))
        \s@(Chunks (c:cs)) -> ungetStream s
    \s -> case s of EOF -> ungetStream s; Chunks [] -> ungetStream s; Chunks (_:_) -> ungetStream s
    ungetStream {- ungetStream is already this strict -}

    --- throwIter ---
    \e -> iterateeToIter (iterToIteratee (throwIter e))
    \e -> iterateeToIter (Iteratee (runEitherT (runStateT (viewT (runIter (throwIter e))) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
    \e -> iterateeToIter (Iteratee (runEitherT (runStateT (viewT (lift (lift (EitherT (return (Left e)))))) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
    \e -> iterateeToIter (Iteratee (runEitherT (runStateT (lift (EitherT (return (Left e)))) (Chunks [])) >>= either (return . Error) (return . viewToStep)))
    \e -> iterateeToIter (Iteratee (runEitherT (EitherT (return (Left e))) >>= either (return . Error) (return . viewToStep)))
    \e -> iterateeToIter (Iteratee (return (Left e) >>= either (return . Error) (return . viewToStep)))
    \e -> iterateeToIter (Iteratee (return (Error e)))
    \e -> lift (return (Error e)) >>= \step -> case step of Continue k -> getStream >>= iterateeToIter . k; Yield x s -> ungetStream s >> return x; Error e -> throwIter e
    \e -> case Error e of Continue k -> getStream >>= iterateeToIter . k; Yield x s -> ungetStream s >> return x; Error e -> throwIter e
    \e -> throwIter e
    throwIter e
    
 -}
-- Hypothesis 2 :: iterToIteratee . iterateeToIter == id
-- (Up to structure representable by "approved" primitives)
{-
    --- return ---
    \x -> iterToIteratee (iterateeToIter (return x))
    \x -> iterToIteratee (iterateeToIter (Iteratee (return (Yield x (Chunks [])))))
    \x -> iterToIteratee (lift (return (Yield x (Chunks []))) >>= \step -> case step of Continue k -> getStream >>= iterateeToIter . k; Yield x s -> ungetStream s >> return x; Error e -> throwIter e)
    \x -> iterToIteratee (ungetStream (Chunks []) >> return x)
    \x -> iterToIteratee (return x)
    \x -> Iteratee (runEitherT (runStateT (viewT (runIter (return x))) (Chunks [])) >>= either (return . Error) (return . viewToStep))
    \x -> Iteratee (runEitherT (runStateT (return (Return x)) (Chunks [])) >>= either (return . Error) (return . viewToStep))
    \x -> Iteratee (runEitherT (return (Return x, Chunks [])) >>= either (return . Error) (return . viewToStep))
    \x -> Iteratee (return (Right (Return x, Chunks [])) >>= either (return . Error) (return . viewToStep))
    \x -> Iteratee (return (viewToStep (Return x, Chunks [])))
    \x -> Iteratee (return (Yield x (Chunks [])))
    \x -> return x
    
    --- fmap ---
    iterToIteratee (iterateeToIter (fmap f x))
    ...
    fmap f x
    (assumed to hold, for now)
    
    --- (>>=) ---
    iterToIteratee (iterateeToIter (x >>= f))
    ...
    x >>= f
    (assumed to hold, for now)
    
    --- fail ---
    (doesn't hold, because Iteratee uses "fail = error" while Iter passes fail up the chain to 'm')
    
    --- lift ---
    iterToIteratee (iterateeToIter (lift x))
    ...
    lift x
    (assumed to hold, for now)
    
    --- Continue ---

    --- Yield ---

    --- Error ---

-}

-- These derivations could probably be greatly simplified by showing that
-- iterateeToIter and iterToIteratee are natural transformations WRT the
-- "approved primitives".  Then, the desired round trip properties
-- follow from trivial derivations, such as:
-- 
--      iterateeToIter (iterToIteratee (return x))
--      iterToIteratee (return x)
--      return x
-- 