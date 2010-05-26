{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, RankNTypes, BangPatterns #-}
module TypeExperiments.RefPrompt where

import TypeExperiments.GCompare
import TypeExperiments.Env as Env
import Control.Monad.Trans (lift)
import Control.Monad.Prompt
import Control.Monad.ST
import Control.Monad.State
import Data.IORef
import Data.STRef
import Data.Typeable
import Data.Word
import Control.Arrow

-- The basic interface for computations using mutable references:
data Refs r t where
    NewRef  :: a -> Refs r (r a)
    ReadRef :: r a -> Refs r a
    WriteRef :: r a -> a -> Refs r ()

newRef :: a -> Prompt (Refs r) (r a)
newRef x = prompt (NewRef x)
readRef :: r a -> Prompt (Refs r) a
readRef r = prompt (ReadRef r)
writeRef :: r a -> a -> Prompt (Refs r) ()
writeRef r x = prompt (WriteRef r x)

-- A few concrete implementations:
ioRefs :: Refs IORef t -> IO t
ioRefs (NewRef x) = newIORef x
ioRefs (ReadRef r) = readIORef r
ioRefs (WriteRef r x) = writeIORef r x

stRefs :: Refs (STRef s) t -> ST s t
stRefs (NewRef x) = newSTRef x
stRefs (ReadRef r) = readSTRef r
stRefs (WriteRef r x) = writeSTRef r x

-- EnvM is a "pure" implementation of something semantically equivalent to ST.
newtype EnvM s t = EnvM {unEnvM :: (State (Word64, Env (Tag s)) t)}
    deriving (Functor, Monad)

runEnvM :: (forall s. EnvM s t) -> t
runEnvM x = evalState (unEnvM x) (0, empty)

envRefs :: Refs (Tag s) t -> EnvM s t
envRefs (NewRef x) = EnvM $ do
    (!n, !s) <- get
    let k = unsafeMkTag n
    put (succ n, insert k x s)
    return k
envRefs (ReadRef k) = EnvM (gets ((Env.! k) . snd))
envRefs (WriteRef k x) = EnvM (modify (id *** insert k x))

-- An example computation making use of the abstract interface:
-- (this _only_ has access to mutable references, nothing else - eg, no printing, no file access, etc.)
gcd_generic a b = do
    a <- newRef a
    b <- newRef b
    
    gcd_loop a b
    
    readRef a
    
gcd_loop a b = loop
    where
        loop = do
            bVal <- readRef b
            if bVal == 0
                then return ()
                else do
                    aVal <- readRef a
                    writeRef a bVal
                    writeRef b (aVal `rem` bVal)
                    loop

-- Wrappers that call the gcd_generic function in various contexts
gcd_env a b = runEnvM (runPromptM envRefs (gcd_generic a b))
gcd_st  a b = runST   (runPromptM stRefs  (gcd_generic a b))
gcd_io  a b =          runPromptM ioRefs  (gcd_generic a b)

-- The last one there, gcd_io, runs in the IO monad but is easily proven "safe"
-- because the type of gcd_generic does not expose any IO operations except
-- those granted explicitly by the ioRefs driver function - which are the creation
-- of references and the use of references previously given.
--
-- In all cases, the operation inherits the safety guarantees of the strongest
-- possible implementation due to parametricity over 'r'.  The mere existence
-- of the EnvM and ST versions proves the safety of the IO version.
-- 
-- Invariants can be checked, or security constraints enforced, at precisely
-- the level of type-detail the types of the prompt expose.  File access 
-- requests could be intercepted and privileges checked, or contents being 
-- written could be examined by a virus-scanner, or whatever.  Contents of a 
-- file or network stream could be transparently compressed, encrypted, 
-- multiplexed, redirected, logged, etc., as data is written.  Buffering 
-- could be done.  All this without necessarily even requiring a concrete 
-- implementation of the prompt operations.  All these things could be 
-- implemented as "implementation transformers".  Sounds an awful lot like 
-- "aspect-oriented programming" to me, only without all the gory splicing 
-- that makes AspectJ et al seem more like glorified macro processors.  
-- The Prompt monad, in effect, does all this splicing too but it just seems 
-- a whole lot more self-contained.  For one thing, it does it at run-time, 
-- opening up a lot of interesting possibilities for dynamically configuring 
-- implementations based on just about anything.