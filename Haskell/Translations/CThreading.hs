-- translations of cthreading.c, a little fragment of (wrong) C code that
-- someone was asking on StackOverflow about how to fix.  The error, as
-- so often happens, is a type error which pretty much any decent static
-- type system would have caught.  As also happens, the opportunity to
-- make the error wouldn't have even occurred in a language like Haskell 
-- or ML due to several factors:
-- 
--      1) Strong typing; there is nothing like (void *) in Haskell (except
--         in the FFI, of course, which lets you shoot yourself in as many
--         feet as you can find, and probably some that you can't).
-- 
--      2) Type inference; In Haskell, a system as unreliable as a *human*
--         is not entrusted with the important task of figuring out whether
--         the program uses values in a way that makes sense.  Sure, they can
--         give the compiler advice, but when it's wrong, the compiler will 
--         still know, and has the final say on whether the code will ever
--         see the light of day.
-- 
--      3) Higher levels of abstraction; In the C code, pthread_create is a
--         sad imitation of a higher-order function.  Because C doesn't offer
--         anything resembling proper support for those, and in particular
--         has no concept whatsoever of polymorphism other than (void *), 
--         the interface is forced to resort to the use of void *.  In
--         Haskell, 'forkIO', et al., are passed fully saturated function 
--         applications which are easier to reason about both for human and
--         for machine.  The type mismatch is "obvious" when you explicitly
--         pass 'i' to the worker before executing it.
module CThreading where

import Control.Async
import Control.Concurrent.MVar
import Data.IORef
import Foreign.C.Types
import Text.Printf

{-

worker_thread lock1 arg = withMVar lock1 $ \_ -> do
    n <- readIORef (arg :: IORef CLLong)
    printf "Testing: %d.\n" (toInteger n)
    

-- nthreads is the total number of threads
nthreads = 1000

main = do
    lock1 <- newMVar ()
    thread_id <- sequence 
        [ do
            -- I actually have to _TRY_ to make a Haskell version have the bug
            -- in the original!  And of course, when I do, the compiler
            -- catches it effortlessly.
            arg <- newIORef (i :: Int)
            forkAsync (worker_thread lock1 arg)
        | i <- [1..nthreads]
        ]
    
    mapM_ waitForAsync thread_id

 -}


-- A slightly more idiomatic version.  Compared to the original C code, 
-- this is far shorter,  much clearer, and incredibly easy to get right.
worker_thread lock n =
    withMVar lock $ \_ -> do
        printf "Testing: %d.\n" (toInteger n)

nthreads = 1000

main = do
    lock <- newMVar ()
    thread_id <- mapM (forkAsync . worker_thread lock) [1..nthreads]
    mapM_ waitForAsync thread_id


