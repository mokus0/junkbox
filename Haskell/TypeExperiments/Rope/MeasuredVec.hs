{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

-- |Simple newtype wrappers for vectors which add 'Measured' instances
-- reporting the length of the vector.
module TypeExperiments.Rope.MeasuredVec where

import Control.Monad
import Data.Monoid
import Data.FingerTree              as FT
import Data.Vector.Generic          as V
import Data.Vector.Generic.Mutable  as MV
import Prelude                      as P

newtype Chunk v a = Chunk { unChunk :: v a }
instance Vector v a => Measured (Sum Int) (Chunk v a) where
    measure (Chunk v) = Sum (V.length v)

newtype ChunkM v s a = ChunkM { unChunkM :: v s a }
instance MVector v a => Measured (Sum Int) (ChunkM v s a) where
    measure (ChunkM v) = Sum (MV.length v)

type instance Mutable (Chunk v) = ChunkM (Mutable v)
instance Vector v a => Vector (Chunk v) a where
    basicUnsafeFreeze (ChunkM v) = liftM Chunk  (basicUnsafeFreeze v)
    basicUnsafeThaw   (Chunk  v) = liftM ChunkM (basicUnsafeThaw   v)
    basicLength       (Chunk  v) = V.basicLength v
    basicUnsafeSlice i j (Chunk v) = Chunk (V.basicUnsafeSlice i j v)
    basicUnsafeIndexM (Chunk v) i = basicUnsafeIndexM v i
instance MVector v a => MVector (ChunkM v) a where
    basicLength       (ChunkM v) = MV.basicLength v
    basicUnsafeSlice i j (ChunkM v) = ChunkM (MV.basicUnsafeSlice i j v)
    basicOverlaps (ChunkM v1) (ChunkM v2) = basicOverlaps v1 v2
    basicUnsafeNew = liftM ChunkM . basicUnsafeNew
    basicUnsafeReplicate n x = liftM ChunkM (basicUnsafeReplicate n x)
    basicUnsafeRead (ChunkM v) i = basicUnsafeRead v i
    basicUnsafeWrite (ChunkM v) i x = basicUnsafeWrite v i x
    basicClear (ChunkM v) = basicClear v
    basicSet (ChunkM v) x = basicSet v x
    basicUnsafeCopy (ChunkM dst) (ChunkM src) = MV.basicUnsafeCopy dst src
    basicUnsafeGrow (ChunkM v) n = liftM ChunkM (basicUnsafeGrow v n)
