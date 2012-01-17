{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- |this is a quick hack as a proof-of-concept for a semi-obvious idea I've never actually 
-- seen attempted: a purely algebraic representation of IO.  I've abstracted the IO functions in
-- the Prelude and Control.Concurrent, since the former is obviously expected and the latter is
-- often cited as one of the most likely suspects when monadicity of IO is challenged.
-- 
-- It's rough around the edges, but I don't believe any of the shortcuts I took are inevitable.
-- For example:
--
-- * In order to make the algebra "open" I've used Typeable hacks.  These can be eliminated by
-- explicitly representing the type of constructors as a coproduct of all the extensions.
--
-- * In order to support 'forkIOWithUnmask' and 'forkOnWithUnmask', I've introduced an abstract type
-- as a parameter of the interface.
-- Conceptually, that type is a primitive type which depends on the eventual implementation context,
-- but in this implementation the type is not held abstract long enough (it should be a member of a 
-- data family, not a parameter of one of the interface types).  This should be fairly easy to 
-- rectify, as described in the comments on that part.
module TypeExperiments.AbstractIO where

import Control.Concurrent hiding 
    ( isCurrentThreadBound, mergeIO, nmergeIO, threadWaitRead, threadWaitWrite, dupChan
    , emptySampleVar, getChanContents, getNumCapabilities, isEmptyChan, isEmptyMVar
    , isEmptySampleVar, killThread, myThreadId, newChan, newEmptyMVar, newEmptySampleVar
    , newMVar, newQSem, newQSemN, newSampleVar, putMVar, readChan, readMVar, readSampleVar
    , signalQSem, signalQSemN, swapMVar, takeMVar, threadCapability, threadDelay, throwTo
    , tryPutMVar, tryTakeMVar, unGetChan, waitQSem, waitQSemN, writeChan, writeList2Chan
    , writeSampleVar, yield
    , forkOS, runInBoundThread, runInUnboundThread, addMVarFinalizer, forkIO, forkIOUnmasked
    , forkIOWithUnmask, forkOn, forkOnWithUnmask, modifyMVar, modifyMVar_, withMVar)
import qualified Control.Concurrent as C
import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Prompt
import Data.GADT.Compare
import Data.Maybe
import Data.Typeable
import System.Posix.Types

import Prelude hiding ( IO, appendFile, catch, getChar, getContents, getLine, interact
                      , ioError, print, putChar, putStr, putStrLn, readFile, readIO
                      , readLn, writeFile)
import qualified Prelude as P

class Typeable1 p => PrimOps p where
class RecPrimOps p where
    typeOfRec :: p m t -> TypeRep
    withRun :: (forall a. m1 a -> m2 a) -> p m1 a -> p m2 a

-- this instance is heinous, but it's less hassle than doing it right and 
-- this is all, after all, a proof of concept.
instance (RecPrimOps p, Typeable1 m) => Typeable1 (p m) where
    typeOf1 = f typeOfRec typeOf1
        where
            f :: (p m t -> TypeRep) -> (m t -> TypeRep) -> p m t -> TypeRep
            f typeOfOp typeOfM _ = mkAppTy (typeOfOp undefined) (typeOfM undefined)

data PrimOp (io :: * -> *) a where
    PrimOp      :: PrimOps    p => !(p    a) -> PrimOp io a
    RecPrimOp   :: RecPrimOps p => !(p io a) -> PrimOp io a

primOp    p = IO (prompt (PrimOp    p))
recPrimOp p = IO (prompt (RecPrimOp (withRun unIO p)))

instance RecPrimOps PrimOp where
    typeOfRec _ = mkTyConApp con []
        where con = mkTyCon3 "abstract-io" "TypeExperiments.AbstractIO" "PrimOp"
    
    -- if the need for this mechanism can be avoided, that would simplify things
    -- a lot.  In order to do that, I'd need something like:
    --
    -- > runRecPromptM :: Monad m => (forall a. p m a -> m a) -> RecPrompt p r -> m r
    --
    -- I suspect that's not generally possible... is it?  It strikes me as very 
    -- similar to functor algebras.  Maybe there's a commutativity equation that
    -- could be exploited.  I'm out of hacking time on this for now though... I
    -- guess that'll be a question for another day.
    withRun run (PrimOp    p) = PrimOp p
    withRun run (RecPrimOp p) = RecPrimOp (withRun run p)

newtype IO a = IO { unIO :: RecPrompt PrimOp a }
    deriving (Typeable, Functor, Monad)

data Lib m where
    Lib     :: Typeable1  p    => (forall t. p   t -> m t) -> Lib m
    RecLib  :: Typeable1 (p m) => (forall t. p m t -> m t) -> Lib m
    Link    :: [Lib m] -> Lib m

runIO :: forall m r. Monad m => (forall a. PrimOp m a -> m a) -> IO r -> m r
runIO f = runRecPromptM (f . withRun (runIO f . IO)) . unIO

data Prelude a where
    AppendFile  :: FilePath -> String -> Prelude ()
    GetChar     ::                       Prelude Char
    GetContents ::                       Prelude String
    GetLine     ::                       Prelude String
    Interact    :: (String -> String) -> Prelude ()
    IoError     ::            IOError -> Prelude a
    Print       :: Show a =>        a -> Prelude ()
    PutChar     ::               Char -> Prelude ()
    PutStr      ::             String -> Prelude ()
    PutStrLn    ::             String -> Prelude ()
    ReadFile    ::           FilePath -> Prelude String
    ReadIO      :: Read a =>   String -> Prelude a
    ReadLn      :: Read a =>             Prelude a
    WriteFile   :: FilePath -> String -> Prelude ()
    deriving Typeable
instance PrimOps Prelude

runPrelude :: Prelude a -> P.IO a
runPrelude (AppendFile f s)     = P.appendFile f s
runPrelude GetChar              = P.getChar
runPrelude GetContents          = P.getContents
runPrelude GetLine              = P.getLine
runPrelude (Interact f)         = P.interact f
runPrelude (IoError e)          = P.ioError e
runPrelude (Print it)           = P.print it
runPrelude (PutChar c)          = P.putChar c
runPrelude (PutStr s)           = P.putStr s
runPrelude (PutStrLn s)         = P.putStrLn s
runPrelude (ReadFile f)         = P.readFile f
runPrelude (ReadIO s)           = P.readIO s
runPrelude ReadLn               = P.readLn
runPrelude (WriteFile f s)      = P.writeFile f s

appendFile  :: FilePath -> String -> IO ()
appendFile f s = primOp (AppendFile f s)

getChar     ::                       IO Char
getChar        = primOp (GetChar)

getContents ::                       IO String
getContents    = primOp (GetContents)

getLine     ::                       IO String
getLine        = primOp (GetLine)

interact    :: (String -> String) -> IO ()
interact f     = primOp (Interact f)

ioError     ::            IOError -> IO a
ioError e      = primOp (IoError e)

print       :: Show a =>        a -> IO ()
print it       = primOp (Print it)

putChar     ::               Char -> IO ()
putChar c      = primOp (PutChar c)

putStr      ::             String -> IO ()
putStr s       = primOp (PutStr s)

putStrLn    ::             String -> IO ()
putStrLn s     = primOp (PutStrLn s)

readFile    ::           FilePath -> IO String
readFile f     = primOp (ReadFile f)

readIO      :: Read a =>   String -> IO a
readIO s       = primOp (ReadIO s)

readLn      :: Read a =>             IO a
readLn         = primOp (ReadLn)

writeFile   :: FilePath -> String -> IO ()
writeFile f s  = primOp (WriteFile f s)

data PreludeRec io a where
    Catch :: io a -> (IOError -> io a) -> PreludeRec io a

runPreludeRec :: PreludeRec P.IO a -> P.IO a
runPreludeRec (Catch x h) = P.catch x h

catch :: IO a -> (IOError -> IO a) -> IO a
catch x h = recPrimOp (Catch x h)

instance RecPrimOps PreludeRec where
    typeOfRec _ = mkTyConApp con []
        where con = mkTyCon3 "abstract-io" "TypeExperiments.AbstractIO" "PreludeRec"
    withRun run (Catch it handle) = Catch (run it) (run . handle)

prelude = Link [Lib runPrelude, RecLib runPreludeRec]

newtype Impl b ta = Impl {runImpl :: ta -> b }
castImpl :: (Typeable1 s, Typeable1 t) => (s a -> b) -> Maybe (t a -> b)
castImpl = fmap runImpl . gcast1 . Impl

tryRunLib :: Typeable1 m => Lib m -> PrimOp m a -> Maybe (m a)
tryRunLib (Lib    l) (PrimOp    p) = fmap ($ p) (castImpl l)
tryRunLib (RecLib l) (RecPrimOp p) = fmap ($ p) (castImpl l)
tryRunLib (Link  ls)            p  = msum [tryRunLib l p | l <- ls]
tryRunLib _ _ = Nothing

runIOWithLib :: (Typeable1 m, Monad m) => Lib m -> IO a -> m a
runIOWithLib l = runIO (fromMaybe linkErr . tryRunLib l) 
    where linkErr = fail "missing implementation for a primop"

data ControlConcurrent a where
    IsCurrentThreadBound    ::                                 ControlConcurrent Bool
    MergeIO                 ::                  [a] -> [a]  -> ControlConcurrent [a]
    NmergeIO                ::                       [[a]]  -> ControlConcurrent [a]
    ThreadWaitRead          ::                          Fd  -> ControlConcurrent ()
    ThreadWaitWrite         ::                          Fd  -> ControlConcurrent ()
    DupChan                 ::                  Chan a      -> ControlConcurrent (Chan a)
    EmptySampleVar          ::                  SampleVar a -> ControlConcurrent ()
    GetChanContents         ::                       Chan a -> ControlConcurrent [a]
    GetNumCapabilities      ::                                 ControlConcurrent Int
    IsEmptyChan             ::                       Chan a -> ControlConcurrent Bool
    IsEmptyMVar             ::                       MVar a -> ControlConcurrent Bool
    IsEmptySampleVar        ::                  SampleVar a -> ControlConcurrent Bool
    KillThread              ::                  ThreadId    -> ControlConcurrent ()
    MyThreadId              ::                                 ControlConcurrent ThreadId
    NewChan                 ::                                 ControlConcurrent (Chan a)
    NewEmptyMVar            ::                                 ControlConcurrent (MVar a)
    NewEmptySampleVar       ::                                 ControlConcurrent (SampleVar a)
    NewMVar                 ::                            a -> ControlConcurrent (MVar a)
    NewQSem                 ::                          Int -> ControlConcurrent QSem
    NewQSemN                ::                          Int -> ControlConcurrent QSemN
    NewSampleVar            ::                            a -> ControlConcurrent (SampleVar a)
    PutMVar                 ::                  MVar a -> a -> ControlConcurrent ()
    ReadChan                ::                       Chan a -> ControlConcurrent a
    ReadMVar                ::                       MVar a -> ControlConcurrent a
    ReadSampleVar           ::                  SampleVar a -> ControlConcurrent a
    SignalQSem              ::                         QSem -> ControlConcurrent ()
    SignalQSemN             ::                 QSemN -> Int -> ControlConcurrent ()
    SwapMVar                ::                  MVar a -> a -> ControlConcurrent a
    TakeMVar                ::                       MVar a -> ControlConcurrent a
    ThreadCapability        ::                     ThreadId -> ControlConcurrent (Int, Bool)
    ThreadDelay             ::                          Int -> ControlConcurrent ()
    ThrowTo                 :: Exception e => ThreadId -> e -> ControlConcurrent ()
    TryPutMVar              ::                  MVar a -> a -> ControlConcurrent Bool
    TryTakeMVar             ::                       MVar a -> ControlConcurrent (Maybe a)
    UnGetChan               ::                  Chan a -> a -> ControlConcurrent ()
    WaitQSem                ::                         QSem -> ControlConcurrent ()
    WaitQSemN               ::                 QSemN -> Int -> ControlConcurrent ()
    WriteChan               ::                  Chan a -> a -> ControlConcurrent ()
    WriteList2Chan          ::                Chan a -> [a] -> ControlConcurrent ()
    WriteSampleVar          ::             SampleVar a -> a -> ControlConcurrent ()
    Yield                   ::                                 ControlConcurrent ()
    deriving Typeable

instance PrimOps ControlConcurrent

runControlConcurrent IsCurrentThreadBound = C.isCurrentThreadBound
runControlConcurrent (MergeIO x y)        = C.mergeIO x y
runControlConcurrent (NmergeIO x)         = C.nmergeIO x
runControlConcurrent (ThreadWaitRead x)   = C.threadWaitRead x
runControlConcurrent (ThreadWaitWrite x)  = C.threadWaitWrite x
runControlConcurrent (DupChan x)          = C.dupChan x
runControlConcurrent (EmptySampleVar x)   = C.emptySampleVar x
runControlConcurrent (GetChanContents x)  = C.getChanContents x
runControlConcurrent GetNumCapabilities   = C.getNumCapabilities
runControlConcurrent (IsEmptyChan x)      = C.isEmptyChan x
runControlConcurrent (IsEmptyMVar x)      = C.isEmptyMVar x
runControlConcurrent (IsEmptySampleVar x) = C.isEmptySampleVar x
runControlConcurrent (KillThread x)       = C.killThread x
runControlConcurrent MyThreadId           = C.myThreadId
runControlConcurrent NewChan              = C.newChan
runControlConcurrent NewEmptyMVar         = C.newEmptyMVar
runControlConcurrent NewEmptySampleVar    = C.newEmptySampleVar
runControlConcurrent (NewMVar x)          = C.newMVar x
runControlConcurrent (NewQSem x)          = C.newQSem x
runControlConcurrent (NewQSemN x)         = C.newQSemN x
runControlConcurrent (NewSampleVar x)     = C.newSampleVar x
runControlConcurrent (PutMVar x y)        = C.putMVar x y
runControlConcurrent (ReadChan x)         = C.readChan x
runControlConcurrent (ReadMVar x)         = C.readMVar x
runControlConcurrent (ReadSampleVar x)    = C.readSampleVar x
runControlConcurrent (SignalQSem x)       = C.signalQSem x
runControlConcurrent (SignalQSemN x y)    = C.signalQSemN x y
runControlConcurrent (SwapMVar x y)       = C.swapMVar x y
runControlConcurrent (TakeMVar x)         = C.takeMVar x
runControlConcurrent (ThreadCapability x) = C.threadCapability x
runControlConcurrent (ThreadDelay x)      = C.threadDelay x
runControlConcurrent (ThrowTo x y)        = C.throwTo x y
runControlConcurrent (TryPutMVar x y)     = C.tryPutMVar x y
runControlConcurrent (TryTakeMVar x)      = C.tryTakeMVar x
runControlConcurrent (UnGetChan x y)      = C.unGetChan x y
runControlConcurrent (WaitQSem x)         = C.waitQSem x
runControlConcurrent (WaitQSemN x y)      = C.waitQSemN x y
runControlConcurrent (WriteChan x y)      = C.writeChan x y
runControlConcurrent (WriteList2Chan x y) = C.writeList2Chan x y
runControlConcurrent (WriteSampleVar x y) = C.writeSampleVar x y
runControlConcurrent Yield                = C.yield

isCurrentThreadBound = primOp (IsCurrentThreadBound)
mergeIO x y          = primOp (MergeIO x y)
nmergeIO x           = primOp (NmergeIO x)
threadWaitRead x     = primOp (ThreadWaitRead x)
threadWaitWrite x    = primOp (ThreadWaitWrite x)
dupChan x            = primOp (DupChan x)
emptySampleVar x     = primOp (EmptySampleVar x)
getChanContents x    = primOp (GetChanContents x)
getNumCapabilities   = primOp (GetNumCapabilities)
isEmptyChan x        = primOp (IsEmptyChan x)
isEmptyMVar x        = primOp (IsEmptyMVar x)
isEmptySampleVar x   = primOp (IsEmptySampleVar x)
killThread x         = primOp (KillThread x)
myThreadId           = primOp (MyThreadId)
newChan              = primOp (NewChan)
newEmptyMVar         = primOp (NewEmptyMVar)
newEmptySampleVar    = primOp (NewEmptySampleVar)
newMVar x            = primOp (NewMVar x)
newQSem x            = primOp (NewQSem x)
newQSemN x           = primOp (NewQSemN x)
newSampleVar x       = primOp (NewSampleVar x)
putMVar x y          = primOp (PutMVar x y)
readChan x           = primOp (ReadChan x)
readMVar x           = primOp (ReadMVar x)
readSampleVar x      = primOp (ReadSampleVar x)
signalQSem x         = primOp (SignalQSem x)
signalQSemN x y      = primOp (SignalQSemN x y)
swapMVar x y         = primOp (SwapMVar x y)
takeMVar x           = primOp (TakeMVar x)
threadCapability x   = primOp (ThreadCapability x)
threadDelay x        = primOp (ThreadDelay x)
throwTo x y          = primOp (ThrowTo x y)
tryPutMVar x y       = primOp (TryPutMVar x y)
tryTakeMVar x        = primOp (TryTakeMVar x)
unGetChan x y        = primOp (UnGetChan x y)
waitQSem x           = primOp (WaitQSem x)
waitQSemN x y        = primOp (WaitQSemN x y)
writeChan x y        = primOp (WriteChan x y)
writeList2Chan x y   = primOp (WriteList2Chan x y)
writeSampleVar x y   = primOp (WriteSampleVar x y)
yield                = primOp (Yield)

data ControlConcurrentRec io a where
    ForkOS                  ::                      io () -> ControlConcurrentRec io ThreadId
    RunInBoundThread        ::                      io a  -> ControlConcurrentRec io a
    RunInUnboundThread      ::                      io a  -> ControlConcurrentRec io a
    AddMVarFinalizer        ::            MVar a -> io () -> ControlConcurrentRec io ()
    ForkIO                  ::                      io () -> ControlConcurrentRec io ThreadId
    ForkIOUnmasked          ::                      io () -> ControlConcurrentRec io ThreadId
    ForkOn                  ::               Int -> io () -> ControlConcurrentRec io ThreadId
    ModifyMVar              :: MVar a -> (a -> io (a, b)) -> ControlConcurrentRec io b
    ModifyMVar_             ::      MVar a -> (a -> io a) -> ControlConcurrentRec io ()
    WithMVar                ::      MVar a -> (a -> io b) -> ControlConcurrentRec io b

instance RecPrimOps ControlConcurrentRec where
    typeOfRec _ = mkTyConApp con []
        where con = mkTyCon3 "abstract-io" "TypeExperiments.AbstractIO" "ControlConcurrentRec"
    
    withRun run (ForkOS             x) = ForkOS (run x)
    withRun run (RunInBoundThread   x) = RunInBoundThread (run x)
    withRun run (RunInUnboundThread x) = RunInUnboundThread (run x)
    withRun run (AddMVarFinalizer x y) = AddMVarFinalizer x (run y)
    withRun run (ForkIO             x) = ForkIO (run x)
    withRun run (ForkIOUnmasked     x) = ForkIOUnmasked (run x)
    withRun run (ForkOn           x y) = ForkOn x (run y)
    withRun run (ModifyMVar       x y) = ModifyMVar x (run . y)
    withRun run (ModifyMVar_      x y) = ModifyMVar_ x (run . y)
    withRun run (WithMVar         x y) = WithMVar x (run . y)

forkOS             x = recPrimOp (ForkOS             x)
runInBoundThread   x = recPrimOp (RunInBoundThread   x)
runInUnboundThread x = recPrimOp (RunInUnboundThread x)
addMVarFinalizer x y = recPrimOp (AddMVarFinalizer x y)
forkIO             x = recPrimOp (ForkIO             x)
forkIOUnmasked     x = recPrimOp (ForkIOUnmasked     x)
forkOn           x y = recPrimOp (ForkOn           x y)
modifyMVar       x y = recPrimOp (ModifyMVar       x y)
modifyMVar_      x y = recPrimOp (ModifyMVar_      x y)
withMVar         x y = recPrimOp (WithMVar         x y)

runControlConcurrentRec (ForkOS             x) = C.forkOS x
runControlConcurrentRec (RunInBoundThread   x) = C.runInBoundThread x
runControlConcurrentRec (RunInUnboundThread x) = C.runInUnboundThread x
runControlConcurrentRec (AddMVarFinalizer x y) = C.addMVarFinalizer x y
runControlConcurrentRec (ForkIO             x) = C.forkIO x
runControlConcurrentRec (ForkIOUnmasked     x) = C.forkIOUnmasked x
runControlConcurrentRec (ForkOn           x y) = C.forkOn x y
runControlConcurrentRec (ModifyMVar       x y) = C.modifyMVar x y
runControlConcurrentRec (ModifyMVar_      x y) = C.modifyMVar_ x y
runControlConcurrentRec (WithMVar         x y) = C.withMVar x y

-- The "withUnmask" functions are problematic...  
-- my chosen solution is to let them be a member of an abstract primitive type
-- which is required to be a subobject of (io a -> ControlConcurrentRec io a).
--
-- this is not really an especially nice solution, because it requires that terms be 
-- constructed with knowledge of the eventual implementation (specifically, knowledge
-- of the type of the unmask operation).  I think this is actually only a problem
-- with the Haskell types, though - specifically the fact that I didn't want to add 
-- any type parameters to IO. Letting the final evaluation context appear as one
-- (i.e, runIO :: Monad m => (forall a. PrimOp m a -> m a) -> IO m r -> m r),
-- it would be pretty easy to make the selection automatic (e.g. as 'data Family 
-- Unmask m').  Categorically, it just means there is another primitive sort which
-- the interpretation functor is free to map to whatever object in the model is
-- appropriate.
data ControlConcurrentUnmask unmask io a where
    ForkIOWithUnmask        ::        (unmask -> io ()) -> ControlConcurrentUnmask unmask io ThreadId
    ForkOnWithUnmask        :: Int -> (unmask -> io ()) -> ControlConcurrentUnmask unmask io ThreadId
    WithUnmask              ::           unmask -> io a -> ControlConcurrentUnmask unmask io a

newtype IOUnmask = IOUnmask { ioUnmask :: forall a. P.IO a -> P.IO a } deriving Typeable
runControlConcurrentUnmask (ForkIOWithUnmask   x) = C.forkIOWithUnmask (x . IOUnmask)
runControlConcurrentUnmask (ForkOnWithUnmask x y) = C.forkOnWithUnmask x (y . IOUnmask)
runControlConcurrentUnmask (WithUnmask       x y) = ioUnmask x y

-- fix the type for P.IO... different instances of these operations would have to be used
-- if you wanted to interpret the IO in some other context.
withIOUnmask :: IOUnmask -> IO a -> IO a
withIOUnmask x y = recPrimOp (WithUnmask x y)
forkIOWithUnmask   x = recPrimOp (ForkIOWithUnmask   (x . withIOUnmask))
forkOnWithUnmask x y = recPrimOp (ForkOnWithUnmask x (y . withIOUnmask))

instance Typeable unmask => RecPrimOps (ControlConcurrentUnmask unmask) where
    typeOfRec = f typeOf
        where 
            con = mkTyCon3 "abstract-io" "TypeExperiments.AbstractIO" "ControlConcurrentUnmask"
            f :: (unmask -> TypeRep) -> ControlConcurrentUnmask unmask io a -> TypeRep
            f typeOfUnmask _ = mkTyConApp con [typeOfUnmask undefined]
    
    withRun run (ForkIOWithUnmask   x) = ForkIOWithUnmask (\unmask -> run (x undefined))
    withRun run (ForkOnWithUnmask x y) = ForkOnWithUnmask x (\unmask -> run (y undefined))
    withRun run (WithUnmask       x y) = WithUnmask x (run y)

controlConcurrent = Link [Lib runControlConcurrent, RecLib runControlConcurrentRec, RecLib runControlConcurrentUnmask]

