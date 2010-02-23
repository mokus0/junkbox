{-# LANGUAGE
        FlexibleInstances
  #-}
module TypeExperiments.RvarT where

-- common imports
import Control.Monad
import Data.Random
import Data.Random.Lift

-- imports for IO version
import Data.IORef

-- imports for MTL State version
import Control.Monad.State hiding (lift)
import qualified Control.Monad.Trans as MTL (lift)
import System.Random

rwalkIO :: RVar Double -> IO (RVarT IO Double)
rwalkIO d = do
    lastVal <- newIORef 0
    
    let x = do
            prev    <- lift (readIORef lastVal)
            change  <- lift d
            
            let new = prev + change
            lift (writeIORef lastVal new)
            return new
        
    return x

main = do
    x <- rwalkIO stdNormal
    sequence_
        [ do
            x' <- sampleFrom DevURandom x
            print x'
        | n <- [1..1000]
        ]

-- If you prefer to use MTL's state classes, things can get quite a bit 
-- trickier.
--
-- What you may have tripped over is the type ambiguity of "get" and "put".
-- Without the type annotations in the commented lines below, 
-- "Data.Random.Lift.lift" and "get" cannot work together because the type
-- of the monad in which to do the "get" is ambiguous.
-- 
-- This form of lift has an extremely general type and is used primarily to
-- support "sample".  Its excessive generality is the main reason it's not
-- exported from Data.Random.  RVarT is, however, an instance of 
-- Control.Monad.Trans.MonadTrans, which in most cases is the preferred way
-- to do the lifting.
--
-- You're better off in most cases using Control.Monad.Trans.lift,
-- which will work in this example without annotations.  Lifting of normal
-- "RVar"s to your RVarT can be accomplished either with Data.Random.Lift.lift
-- as above or, to avoid cluttering the namespace, I personally prefer to use
-- "sample".
--
-- Note also that no generator needs to be threaded through: this is done
-- after the fact by the sampling operation, in a way exactly analogous
-- to the prompt-substitution in MonadPrompt.
rwalkState :: RVar Double -> RVarT (State Double) Double
rwalkState d = do
    prev <- MTL.lift get
    change  <- sample d
    
    let new = prev + change
    MTL.lift (put new)
    return new

-- one final stumbling-block I noticed working this out is that sample 
-- inherits some of the overgenerality problems from lift, meaning that 
-- something must be done to fix the type of the monad into which is being 
-- sampled.  That can usually be inferred from the context, but in the 
-- "main2" below the type of "0" is ambiguous and 'sample' doesn't do enough 
-- to nail it down, so I've done so explicitly.  "runRVarT" could be used in
-- place of "sample" but it has the same problem.

main2 :: IO ()
main2 = do
    gen <- newStdGen
    let x = rwalkState stdNormal
    
        xs = evalState (evalStateT (sample (replicateM 1000 x)) gen) (0 :: Double)

    mapM_ print xs

