{-# LANGUAGE
        GADTs,
        MultiParamTypeClasses,
        ForeignFunctionInterface
  #-}
module Math.CRandom where

import Control.Concurrent.MVar
import Data.Bits
import Data.Random.Source
import Data.Word
import Foreign.C.Types
import System.IO.Unsafe

data CRandom = CRandom

instance RandomSource IO CRandom where
    supportedPrimsFrom _ PrimWord8  = True
    supportedPrimsFrom _ PrimWord16 = True
    supportedPrimsFrom _ PrimWord32 = True
    supportedPrimsFrom _ (PrimNByteInteger _) = True
    supportedPrimsFrom _ _ = False
    
    getSupportedRandomPrimFrom _ PrimWord8  = fromIntegral `fmap` sip 8
    getSupportedRandomPrimFrom _ PrimWord16 = fromIntegral `fmap` sip 16
    getSupportedRandomPrimFrom _ PrimWord32 = fromIntegral `fmap` sip 32
    getSupportedRandomPrimFrom _ (PrimNByteInteger n) = slurp (n `shiftL` 3)

data Stash = Stash !Int !Word64 deriving Show
stash = unsafePerformIO (newMVar (Stash 0 0))

foreign import ccall "stdlib.h random" c_random :: IO CInt

slurp (n+32)    = do
    x <- sip 32
    xs <- slurp n
    return $! ((toInteger x `shiftL` n) .|. xs)
slurp n         = toInteger `fmap` sip n

-- never sip more than 32 bits!
sip 0 = return 0
sip bits = modifyMVar stash $ \stash -> do
    Stash n stash <- refill bits stash
    return (Stash (n - bits) (stash `shiftR` bits), stash .&. (bit bits - 1))

refill bits (Stash n stash) 
    | bits <= n = return (Stash n stash)
    | otherwise = do
        more <- c_random
        
        refill bits (Stash (n + 31) (stash .|. (fromIntegral more `shiftL` n)))