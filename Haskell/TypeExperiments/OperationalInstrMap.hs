{-# LANGUAGE RankNTypes, GADTs, GeneralizedNewtypeDeriving #-}
module TypeExperiments.OperationalInstrMap where

import Control.Applicative
import Control.Monad.Operational
import Control.Monad.State
import Control.Monad.Trans

mapInstr :: Monad m => (forall t. instr1 t -> instr2 t) -> ProgramT instr1 m a -> ProgramT instr2 m a
mapInstr f p = do
    v <- lift (viewT p)
    case v of
        Return x -> return x
        x :>>= g -> singleton (f x) >>= mapInstr f . g

data Fetch sym a where
    Fetch :: Fetch sym (Stream sym)

data Stream sym = EOF | Chunks [sym] deriving (Eq, Show)

newtype Iter sym m a = Iter { unIter :: ProgramT (Fetch sym) (StateT (Stream sym) m) a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans (Iter sym) where
    lift = Iter . lift . lift

getInput :: Monad m => Iter sym m (Stream sym)
getInput = do
    stashed <- lookahead
    Iter $ if isEmpty stashed
        then do
            input <- singleton Fetch
            lift (put (takeStream 0 input))
            return input
        else do
            lift (put (Chunks []))
            return stashed

lookahead :: Monad m => Iter sym m (Stream sym)
lookahead = Iter (lift get)

getSymbols n = do
    input <- getInput
    if isEOF input
        then return EOF
        else do
            let (result, rest)  = splitStreamAt n input
                nResults        = streamLength result
            Iter (lift (put rest))
            if isEmpty rest && nResults < n
                then do
                    more <- getSymbols (n - nResults)
                    return (appendStream result more)
                else return result

streamToList EOF         = []
streamToList (Chunks cs) = cs

streamLength  str = length  (streamToList str)
isEmpty       str = null    (streamToList str)

isEOF    EOF          = True
isEOF    _            = False

appendStream s1   s2
    | isEOF s1 && isEOF s2  = EOF
    | otherwise             = Chunks (streamToList s1 ++ streamToList s2)

splitStreamAt n EOF          = (EOF,        EOF        )
splitStreamAt n (Chunks cs)  = (Chunks xs,  Chunks ys  )
    where (xs,ys) = splitAt n cs

takeStream  n = fst  . splitStreamAt n
dropStream  n = snd  . splitStreamAt n

-- feed :: Stream sym -> Iter sym m a -> m (Iter sym m a)
-- feed s (Iter x) = do
--     v <- viewT x
--     case v of 