{-# LANGUAGE 
    GADTs, GeneralizedNewtypeDeriving, RankNTypes, 
    BangPatterns, 
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances
  #-}
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
import Control.Arrow ((***))
import Control.Monad.LoopWhile

-- The basic interface for computations using mutable references:
-- |The internal "Prompt" type - the fundamental language provided by which
-- clients can talk about what they want to do with references.
data Refs r t where
    NewRef  :: a -> Refs r (r a)
    ReadRef :: r a -> Refs r a
    WriteRef :: r a -> a -> Refs r ()

-- Nice wrapper functions for clients to use:
-- |Create a new reference containing the given value
newRef x = prompt (NewRef x)
-- |Read the value contained in an existing reference
readRef r = prompt (ReadRef r)
-- |Replace the value in an existing reference with something new
writeRef r x = prompt (WriteRef r x)

-- A few concrete implementations (translations from the prompt language to
-- various existing target languages):
ioRefs :: Refs IORef t -> IO t
ioRefs (NewRef x) = newIORef x
ioRefs (ReadRef r) = readIORef r
ioRefs (WriteRef r x) = writeIORef r x

stRefs :: Refs (STRef s) t -> ST s t
stRefs (NewRef x) = newSTRef x
stRefs (ReadRef r) = readSTRef r
stRefs (WriteRef r x) = writeSTRef r x

-- EnvM is a "pure" implementation of something semantically equivalent to ST.
newtype EnvM s t = EnvM {unEnvM :: (State (Integer, Env (Tag s)) t)}
    deriving (Functor, Monad)

runEnvM :: (forall s. EnvM s t) -> t
runEnvM x = evalState (unEnvM x) (0, empty)

-- |A 'Refs' implementation using 'EnvM'
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
-- The 'loop' and 'while' functions are from Control.Monad.LoopWhile, a pretty
-- simple and elegant loop monad transformer.
gcd_generic a b = do
    a <- newRef a
    b <- newRef b
    
    loop $ do
        bVal <- readRef b
        while (bVal /= 0)
        aVal <- readRef a
        writeRef a bVal
        writeRef b (aVal `rem` bVal)
        
    readRef a

-- In order to make the above code a bit more clean, the LoopWhileT 
-- transformer is defined here as preserving the prompt, so that the
-- reference operations do not need to be explicitly lifted inside the 
-- loop body.  This is arguably a bad precedent though, as it hides a
-- (admittedly very small) semantic shift.
-- 
-- I think it is a justifiable case though, because many funtions
-- making use of mutable references will also make use of loops.  After
-- all, without looping there's not much point in using mutable state
-- in the first place.
-- 
-- To avoid introducing an orphan instance, only the Refs prompt is lifted.
instance MonadPrompt (Refs r) m => MonadPrompt (Refs r) (LoopWhileT m) where
    prompt = lift . prompt


-- Wrappers that call the gcd_generic function with various concrete 
-- implementations of references.
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