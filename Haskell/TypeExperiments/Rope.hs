{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |I have not ever seen any other language in which such simple concepts
-- can be composed so trivially to produce a coherent, high-level whole.
module TypeExperiments.Rope
    ( Rope
    , fromChunks, toChunks
    , append
    , splitRopeAt
    , insertAt, deleteAt, replaceAt
    , RopeM
    , fromChunksM, toChunksM
    , appendM
    , splitRopeMAt
    , insertAtM, deleteAtM, replaceAtM
    ) where

import TypeExperiments.Rope.FTUtils as FT
import TypeExperiments.Rope.MeasuredVec

import Control.Monad
import Data.Monoid
import Data.FingerTree              as FT
import Data.Foldable                as F
import Data.Vector.Generic          as V
import Data.Vector.Generic.Mutable  as MV
import Prelude                      as P

newtype Rope  v   a = Rope  { ropeFT  :: FingerTree (Sum Int) (Chunk  v   a) }
newtype RopeM v s a = RopeM { ropeMFT :: FingerTree (Sum Int) (ChunkM v s a) }

fromChunks :: Vector v a => [v a] -> Rope v a
fromChunks = Rope . FT.fromList . P.map Chunk

toChunks :: Rope v a -> [v a]
toChunks = P.map unChunk . F.toList . ropeFT

fromChunksM :: MVector v a => [v s a] -> RopeM v s a
fromChunksM = RopeM . FT.fromList . P.map ChunkM

toChunksM :: RopeM v s a -> [v s a]
toChunksM = P.map unChunkM . F.toList . ropeMFT

type instance Mutable (Rope v) = RopeM (Mutable v)
instance Vector v a => Vector (Rope v) a where
    basicUnsafeFreeze (RopeM vs) = liftM Rope  (FT.unsafeMapM basicUnsafeFreeze vs)
    basicUnsafeThaw   (Rope  vs) = liftM RopeM (FT.unsafeMapM basicUnsafeThaw   vs)
    
    basicLength (Rope vs) = getSum (measure vs)
    
    basicUnsafeSlice start len (Rope r) = Rope
        (sliceFingerTreeBy basicUnsafeSplitV start len r)
    
    basicUnsafeIndexM rope i = do
        let (_, Rope b) = splitRopeAt i rope
        segment <- FT.headM b
        basicUnsafeIndexM segment 0

instance MVector v a => MVector (RopeM v) a where
    basicLength (RopeM vs) = getSum (measure vs)
    
    basicUnsafeSlice start len (RopeM r) = RopeM
        (sliceFingerTreeBy basicUnsafeSplitMV start len r)
    
    basicOverlaps (RopeM v1) (RopeM v2)
        = F.any (flip F.any v2 . basicOverlaps) v1
    
    basicUnsafeNew 0 = return (RopeM FT.empty)
    basicUnsafeNew n = do
        v <- basicUnsafeNew n
        return (RopeM (FT.singleton v))
    
    basicUnsafeRead rope i = do
        let (_, RopeM b) = splitRopeMAt i rope
        segment <- FT.headM b
        basicUnsafeRead segment 0
    
    basicUnsafeWrite rope i x = do
        let (_, RopeM b) = splitRopeMAt i rope
        segment <- FT.headM b
        basicUnsafeWrite segment 0 x
    
    basicUnsafeGrow r 0 = return r
    basicUnsafeGrow (RopeM ft) n = do
        v <- basicUnsafeNew n
        return (RopeM (ft |> v))

basicUnsafeSplitV :: Vector v a => Int -> v a -> (v a, v a)
basicUnsafeSplitV i v = (V.basicUnsafeSlice 0 i v, V.basicUnsafeSlice i (n-i) v)
    where n = V.basicLength v

basicUnsafeSplitMV :: MVector v a => Int -> v s a -> (v s a, v s a)
basicUnsafeSplitMV i v = (MV.basicUnsafeSlice 0 i v, MV.basicUnsafeSlice i (n-i) v)
    where n = MV.basicLength v

splitRopeAt :: Vector v a => Int -> Rope v a -> (Rope v a, Rope v a)
splitRopeAt i (Rope ft) = 
    let (l, r) = splitFingerTreeBy basicUnsafeSplitV i ft
     in (Rope l, Rope r)

splitRopeMAt :: MVector v a => Int -> RopeM v s a -> (RopeM v s a, RopeM v s a)
splitRopeMAt i (RopeM ft) = 
    let (l, r) = splitFingerTreeBy basicUnsafeSplitMV i ft
     in (RopeM l, RopeM r)

append :: Vector v a => Rope v a -> Rope v a -> Rope v a
append (Rope v1) (Rope v2) = Rope (v1 >< v2)

appendM :: MVector v a => RopeM v s a -> RopeM v s a -> RopeM v s a
appendM (RopeM v1) (RopeM v2) = RopeM (v1 >< v2)

insertAt  :: Vector v a => Int -> Rope v a -> Rope v a -> Rope v a
insertAt at this that = P.foldl1 append [before, this, after]
    where (before, after) = splitRopeAt at that

insertAtM  :: MVector v a => Int -> RopeM v s a -> RopeM v s a -> RopeM v s a
insertAtM at this that = P.foldl1 appendM [before, this, after]
    where (before, after) = splitRopeMAt at that

deleteAt :: Vector v a => Int -> Int -> Rope v a -> Rope v a
deleteAt start len rope = append before after
    where
        (before, rest) = splitRopeAt start rope
        (_mid,  after) = splitRopeAt len rest

deleteAtM :: MVector v a => Int -> Int -> RopeM v s a -> RopeM v s a
deleteAtM start len rope = appendM before after
    where
        (before, rest) = splitRopeMAt start rope
        (_mid,  after) = splitRopeMAt len rest

replaceAt :: Vector v a => Int -> Int -> Rope v a -> Rope v a -> Rope v a
replaceAt start len this = insertAt start this . deleteAt start len

replaceAtM :: MVector v a => Int -> Int -> RopeM v s a -> RopeM v s a -> RopeM v s a
replaceAtM start len this = insertAtM start this . deleteAtM start len

