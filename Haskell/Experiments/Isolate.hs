{-# LANGUAGE GADTs #-}
-- | Here's a fun little example of why Haskell is the greatest imperative language around :)
-- 
-- This is a high-level interface for creating a network of interacting processes.
-- Those processes are given as Haskell IO procedures which are then executed on different
-- OS processes.  No run-time compilation is needed (though in theory there's nothing 
-- stopping you from using it too) and the boundaries between processes have enforced
-- type-safety (though of course there's the added possibility of transport errors).
-- 
-- It hides all the messy details and just gives a nice interface for building and executing
-- a high-level communication network.  Just about anything that's easy to do at the OS-process
-- level would be ridiculously easy to add here too (resource limits are provided as an example).
--
-- other limitations that could fairly easily be imposed on a per-'Proc' basis:
--  - Chroot / privilege reduction
--  - time limits (either CPU or wall time)
--  - process "nice" level
--  - time share limits (max CPU time per wall time, regulated by forcibly pausing
--    the process several times per second, when needed)
--  - a fully locked down sandbox "preset" you can use to run totally untrusted native code
--
-- problems with current implementation:
--  - Little to no thought has been given to error handling - this is purely a toy
--    implementation.  Asynchronous exceptions in particular can probably wreak havoc.
--  - Chans can fill up way faster than their contents can be pushed over the pipes,
--    and nothing is ensuring that they get flushed before a process terminates.
--  - Chans are not really all that great an abstraction for this purpose to begin with.
--    TChans would be marginally better since they at least support a more natural
--    'select'-like operation.  Even better would be something that explicitly specifies
--    direction of flow.  There also is no concept of 'response to a particular message',
--    which would be nice to support.
--  - when chaining 'withChan', channels are not supplied in the same lexicographic order as
--    the arguments they correspond to.  This is likely to be confusing to some people
--    and annoying to the rest.  I think providing the "expected" order in the convenience
--    functions 'with2Chans' et al. confuses the issue more than it helps.  I have a truly
--    marvelous solution to this problem which this margin is too narrow to contain ;).
--  - There is some kind of CPU thrashing and i'm not sure whether it's in the Chan handling code
--    or the I/O.  I'm also not sure whether it is because of something I've done wrong or 
--    something the code I'm calling does wrong.  I don't plan to track it down tonight.
--  - The network is completely static and there is a single central "router" process which
--    is not especially robust.
--  - There is no way to monitor the health of the network or any part of it.
--  - The code is a late-night hack.  It's ugly and only superficially tested.  It's 
--    probably obscenely slow and it probably leaks memory.
--
-- Ideally, something like this library would be expanded to implement many of the ideas
-- of Erlang's OTP, including distribution of processes across multiple physical machines.
-- I'm actually pretty sure that most of that could be done while maintaining the same 
-- ease of use, which is not something for which I'd hold much hope in any other language.

module Experiments.Isolate
     ( Proto(..), defaultProto
     , Proc, proc, limit, limits, withChan, with2Chans, with3Chans, withChanProto, run
     ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import qualified Control.Concurrent.Thread.Group as TGrp
import qualified Data.ByteString as BS
import Data.IORef
import Data.Serialize
import System.IO
import System.Posix.IO
import System.Posix.Process
import System.Posix.Resource
import System.Posix.Types
import qualified Data.Dependent.Map as D
import qualified Data.Map as M
import qualified Data.Set as S

data Proto t = Proto
    { decoder    :: Get t
    , encoder    :: t -> Put
    }

defaultProto :: Serialize t => Proto t
defaultProto = Proto get put

data Proc c t where
    Proc :: t -> Proc c t
    Limit :: Resource -> ResourceLimits -> Proc c t -> Proc c t
    WithChan :: c a -> Proto a -> Proc c (Chan a -> b) -> Proc c b

instance Functor (Proc c) where
    fmap f (Proc p) = Proc (f p)
    fmap f (Limit r l p) = Limit r l (fmap f p)
    fmap f (WithChan c cProto p) = WithChan c cProto (fmap (fmap f) p)

proc p = Proc p

limit r l p = Limit r l p

limits [] = id
limits ((r, l) : ls) = limit r l . limits ls

withChan c = WithChan c defaultProto
with2Chans xs ys = withChan ys . withChan xs
with3Chans xs ys zs = withChan zs . with2Chans xs ys
withChanProto c p = WithChan c p

type Pipes c = M.Map (D.Key c) (Handle, Handle) -- collection of bidirectional pipes: (read end, write end)
openPipes :: D.GCompare c => Proc c t -> IO (Pipes c, Pipes c)
openPipes = loop M.empty M.empty
    where 
        loop :: D.GCompare c => Pipes c -> Pipes c -> Proc c t -> IO (Pipes c, Pipes c)
        loop ins outs Proc{} = return (ins, outs)
        loop ins outs (Limit _ _ p) = loop ins outs p
        loop ins outs (WithChan c _ p) = do
            (r1,w2) <- createPipe
            (r2,w1) <- createPipe
            r1 <- fdToHandle r1
            r2 <- fdToHandle r2
            w1 <- fdToHandle w1
            w2 <- fdToHandle w2
            
            sequence_
                [ do
                    hSetBuffering h NoBuffering
                    hSetBinaryMode h True
                | h <- [r1, r2, w1, w2]
                ]
            
            loop (M.insert (D.Key c) (r1, w1) ins)
                 (M.insert (D.Key c) (r2, w2) outs)
                 p

closePipes :: Pipes c -> IO ()
closePipes = mapM_ hCloseBoth . M.elems
    where hCloseBoth (r,w) = hClose r >> hClose w

runProc :: D.GCompare c => Pipes c -> Proc c (IO t) -> IO t
runProc pipes = join . buildProc pipes

buildProc :: D.GCompare c => Pipes c -> Proc c t -> IO t
buildProc pipes (Proc p) = return p
buildProc pipes (Limit res lim proc) = do
    setResourceLimit res lim
    buildProc pipes proc
buildProc pipes (WithChan cName cProto proc) = do
    chan <- newChan
    
    let (pipeR, pipeW) = pipes M.! D.Key cName
        
        restartParserWith bs = feed bs (Partial (runGetPartial (decoder cProto)))
        
        feed chunk (Fail str) = Fail str
        feed chunk (Partial k)
             | BS.null chunk    = Partial k
             | otherwise        = k chunk
        feed chunk (Done x rest) = Done x (BS.append rest chunk)
    
    decoderState <- newIORef (restartParserWith BS.empty)
    let drain into = do
            state <- readIORef decoderState
            case state of
                Fail str  -> fail str
                Partial k -> return ()
                Done x rest -> do
                    writeIORef decoderState (restartParserWith rest)
                    into x
                    drain into
        
        bufSz = 4096
        
        readPipe  = BS.hGetSome pipeR bufSz >>= (modifyIORef decoderState . feed)
        writePipe = BS.hPut pipeW . runPut . encoder cProto
    
    forkIO $ forever $ readPipe >> drain (writeChan chan)
    forkIO $ forever $ readChan chan >>= writePipe
    
    fmap ($chan) (buildProc pipes proc)

type Routes c = M.Map ProcessID (Pipes c)
runRouter :: D.GCompare c => Routes c -> IO ()
runRouter routes = do
    -- for every distinct @Key c@, create a Chan (ProcessID, ByteString)
    let chanIDs = S.unions (map M.keysSet (M.elems routes))
    -- chans :: M.Map (D.Key c) (Chan (ProcessID, ByteString))
    -- TODO: clean this up, over-dups don't seem to have been the cause of the memory leak
    chans <- M.fromAscList <$> sequence [ (,) cId <$> newChan | cId <- S.toAscList chanIDs]
    
    tgrp <- TGrp.new
    
    let bufSz = 4096
    
    -- for every pipe of every process, fork handlers which do the following:
    --  1) get data from the pipe, tag it with the associated process's ID, and push it into the corresponding chan
    --  2) for each chan, makes a dup and reads it forever, ignoring messages tagged with the client's ProcessID and passing all others
    sequence_
        [ do
            let wChan = chans M.! chanId
            rChan <- dupChan wChan
                
            TGrp.forkIO tgrp $ forever $ do
                msg <- BS.hGetSome pipeR bufSz
                if BS.null msg
                    then do
                        eof <- hIsEOF pipeR
                        if eof 
                            then hClose pipeR
                            else fail ("got 0-byte message on pipe from process " ++ show pid)
                                -- ^ this shouldn't happen, right?
                    else writeChan wChan (pid, msg)
            TGrp.forkIO tgrp $ forever $ do
                (msgPid, msg) <- readChan rChan
                when (msgPid /= pid) (BS.hPut pipeW msg)
        | (pid, pipes) <- M.assocs routes
        , (chanId, (pipeR, pipeW)) <- M.toList pipes
        ]
    
    TGrp.wait tgrp

forkProc :: D.GCompare c => Proc c (IO ()) -> IO (ProcessID, Pipes c)
forkProc proc = do
    (inPipes, outPipes) <- openPipes proc
    pid <- forkProcess $ do
        closePipes outPipes
        runProc inPipes proc
    closePipes inPipes
    return (pid, outPipes)

waitForProcesses :: [ProcessID] -> IO ()
waitForProcesses [] = return ()
waitForProcesses ps = do
    stat <- getAnyProcessStatus True False
    let isEnded Exited{}        = True
        isEnded Terminated{}    = True
        isEnded _               = False
    case stat of
        Just (p, pStat)
            | isEnded pStat -> waitForProcesses (filter (/=p) ps)
        _                   -> waitForProcesses ps

-- run a collection of named processes, each of which may communicate to 
-- the others on channels "named" by 'c'.  Each will be run in a separate
-- system-level process and the parent will wait for all the children to terminate
run :: D.GCompare c => M.Map String (Proc c (IO ())) -> IO ()
run procs = do
    let (procNames, procImpls) = unzip (M.toAscList procs)
        
    routes <- mapM forkProc procImpls
    let (pids, pipes) = unzip routes
    
    routerPid <- forkProcess (runRouter (M.fromList routes))
    mapM_ closePipes pipes
    
    waitForProcesses (routerPid : pids)
