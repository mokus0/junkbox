{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module System.VT100Emu.Buffer where

import Control.Monad.Primitive
import TypeExperiments.PrimRef
import TypeExperiments.Rope             as R

import Data.Vector.Generic.Mutable as MV

data Buffer v s a = Buffer
    { position  :: !(STRef s Int)
    , contents  :: !(STRef s (RopeM v s a))
    }

newBuffer :: (PrimMonad m, MVector v a) => m (Buffer v (PrimState m) a)
newBuffer = do
    position <- newSTRef 0
    rope     <- MV.new 0
    contents <- newSTRef rope
    return Buffer{..}

getPosition Buffer{..} = readSTRef  position
setPosition Buffer{..} = writeSTRef position

insert it buf = do
    v <- MV.replicate 1 it
    insertMany v buf

insertMany v Buffer{..} = do
    let it = fromChunksM [v]
        n = MV.length it
    
    pos <- readSTRef position
    modifySTRef contents (R.insertAtM pos it)
    writeSTRef position $! (pos + n)

replace it buf = do
    v <- MV.replicate 1 it
    replaceMany v buf

replaceMany v Buffer{..} = do
    let it = fromChunksM [v]
        n = MV.length it
    
    pos <- readSTRef position
    modifySTRef contents (R.replaceAtM pos n it)
    writeSTRef position $! (pos + n)

edit f Buffer{..} = do
    pos <- readSTRef position
    buf <- readSTRef contents
    
    x <- MV.read buf pos
    f x

withBuffer :: (PrimMonad m, MVector v a) => Buffer v (PrimState m) a -> (forall v2. MVector v2 a => v2 (PrimState m) a -> m b) -> m b
withBuffer Buffer{..} f = do
    pos <- readSTRef position
    buf <- readSTRef contents
    f (MV.slice pos (MV.length buf - pos) buf)
