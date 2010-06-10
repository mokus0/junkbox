{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
module NR.Ch1.S4.MVec where

import NR.Ch1.S4.IVec

import Control.Monad.Primitive
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as UV
import qualified Data.Vector.Generic as GV (unsafeFreeze)
import qualified Data.Vector.Generic.Mutable as GV


class Monad m => MVector v t m where
    newVector_ :: Int -> m (v t)
    newVector  :: Int -> (Int -> t) -> m (v t)
    getVecSize :: v t -> m Int
    readV  :: v t -> Int -> m t
    writeV :: v t -> Int -> t -> m ()
    modifyV :: v t -> Int -> (t -> t) -> m t
    modifyV v i f = do
        x <- readV v i
        let fx = f x
        writeV v i fx
        return fx
    updateV :: v t -> Int -> (t -> m t) -> m t
    updateV m i f = do
        x <- readV m i
        x <- f x
        writeV m i x
        return x

copyVector v = newVector (vecElems v) (indexV v)

swapVecElems v i j = do
    iv <- readV v i
    jv <- readV v j
    writeV      v j iv
    writeV      v i jv

swapVectors v1 v2 = do
    n1 <- getVecSize v1
    n2 <- getVecSize v2
    if n1 /= n2
        then fail "swapVectors: vectors' sizes differ"
        else unsafeSwapVectors n1 v1 v2

{-# INLINE unsafeSwapVectors #-}
unsafeSwapVectors n v1 v2 = sequence_
    [ do
        x1 <- readV v1 i
        x2 <- readV v2 i
        writeV       v1 i x2
        writeV       v2 i x1
    
    | i <- [0..n-1]
    ]

-- |zipWithV_is is vDest f v1 v2:
-- Like zipWith, but operates on vectors.  Operates on indices 'is' and 
-- places results in vDest, which may be the same as v1 or v2.
zipWithV_is is vDest f v1 v2  = sequence_
    [ do
        x <- readV v1 i
        y <- readV v2 i
        writeV vDest i (f x y)
    | i <- is
    ]

zipWithV_n n  = zipWithV_is [0..n-1]
zipWithV vDest f v1 v2 = do
    n1 <- getVecSize v1
    n2 <- getVecSize v2
    nD <- getVecSize vDest
    zipWithV_n (minimum [n1, n2, nD]) vDest f v1 v2


type STVector  s = NRVector (V.MVector  s)
type STUVector s = NRVector (UV.MVector s)

instance (GV.MVector v t, PrimMonad m, s ~ PrimState m) => MVector (NRVector (v s)) t m where
    newVector_ n = do
        v <- GV.new n
        return (NRVector v)
    newVector n f = do
        v <- GV.new n
        sequence_
            [ GV.write v i (f i)
            | i <- [0..n-1]
            ]
        return (NRVector v)
    getVecSize (NRVector v) = return (GV.length v)
    
    readV  (NRVector v) = GV.read v
    writeV (NRVector v) = GV.write v

unsafeFreezeVector (NRVector v) = do
    v <- GV.unsafeFreeze v
    return (NRVector v)
