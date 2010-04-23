{-# LANGUAGE BangPatterns #-}
module TypeExperiments.Uniq (Uniq, getUniq) where

import Control.Monad.Primitive
import Data.IORef
import Data.Word
import System.IO.Unsafe

newtype Uniq s = Uniq Word64 deriving (Eq, Ord)

{-# NOINLINE nextUniq #-}
nextUniq = unsafePerformIO (newIORef 0)

getUniq :: PrimMonad m => m (Uniq (PrimState m))
getUniq = unsafePrimToPrim (atomicModifyIORef nextUniq (\(!u) -> let u' = u+1 in u' `seq` (u', Uniq u)))