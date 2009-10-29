module Static where

import System.IO.Unsafe
import Control.Monad.ST
import Data.STRef
import Data.IORef
import Debug.Trace

-- these functions only get properly inlined with 
-- optimization enabled, therefore the behavior of this program
-- depends very strongly on how it was compiled.
-- 
-- If compiled with -O it prints:
--  > init f
--  > 0
--  > 3
--  > init g
--  > 0
--  > 3
--
-- If compiled without -O it prints:
--  > init f
--  > 0
--  > init f
--  > 0
--  > init g
--  > 0
--  > init g
--  > 0
--
-- Clearly not a desirable behavior...
-- 

{-# INLINE staticST #-}
staticST :: ST s a -> a
staticST init = cell
    where
        {-# NOINLINE cell #-}
        cell = unsafePerformIO (unsafeSTToIO init)

{-# INLINE static #-}
static :: IO a -> a
static init = cell
    where
        {-# NOINLINE cell #-}
        cell = unsafePerformIO init

f :: Int -> IO Int
f x = do 
    res <- readIORef cell
    writeIORef cell x
    return res

    where
        cell = static (trace "init f" (newIORef 0))

g :: Int -> Int
g x = runST $ do 
    res <- readSTRef cell
    writeSTRef cell x
    return res

    where
        cell = staticST (trace "init g" (newSTRef 0))

main = do
    f 3 >>= print
    f 6 >>= print
    print (g 3)
    print (g 6)
    