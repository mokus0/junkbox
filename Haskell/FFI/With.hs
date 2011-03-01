{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FFI.With
    ( With(..), with2, with3, with4, with5
    , ByVal(..)
    , In(..), Out(..), InOut(..)
    ) where

import Control.Monad
import Data.Word
import Foreign (Ptr, Storable(..), alloca, peekArray, pokeArray, castPtr)
import Foreign.C.Types (CSize)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Writer (WriterT(..))
import Control.Monad.Trans.State  (StateT(..))
import Data.Monoid (Monoid, First(..))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Peel (MonadPeelIO, liftIOOp)

newtype ByVal a = ByVal a
newtype In    a = In    (Ptr a)
newtype Out   a = Out   (Ptr a)
newtype InOut a = InOut (Ptr a)

class (Monad m1, Monad m2) => With t m1 m2 | t m1 -> m2 where
    with :: (t -> m1 r) -> m2 r

instance Monad m => With (ByVal t) m (ReaderT t m) where
    with f = ReaderT (f . ByVal)

withIn :: (Storable a, MonadPeelIO m) => (In a -> m b) -> a -> m b
withIn f x = liftIOOp alloca $ \p -> do
    liftIO (poke p x)
    f (In p)

instance (Storable t, MonadPeelIO m) => With (In t) m (ReaderT t m) where
    with = ReaderT . withIn

withOut :: (Storable a, MonadPeelIO m) => (Out a -> m b) -> m (b,a)
withOut f = liftIOOp alloca $ \p -> do
    b <- f (Out p)
    a <- liftIO (peek p)
    return (b,a)

instance (Storable t, MonadPeelIO m) => With (Out t) m (WriterT (First t) m) where
    with f = WriterT $ do
        (b,a) <- withOut f
        return (b, First (Just a))

withInOut :: (Storable a, MonadPeelIO m) => (InOut a -> m b) -> a -> m (b,a)
withInOut f a = liftIOOp alloca $ \p -> do
    liftIO (poke p a)
    b <- f (InOut p)
    a <- liftIO (peek p)
    return (b,a)

instance (Storable t, MonadPeelIO m) => With (InOut t) m (StateT t m) where
    with = StateT . withInOut


-- One can easily marshal any number of arguments at once:
with2 f = with (with  . f)
with3 f = with (with2 . f)
with4 f = with (with3 . f)
with5 f = with (with4 . f)

-- examples: first, some silly bit-twiddling functions of several arities:

foo :: Storable a => In a -> IO [Word8]
foo x = alloca (\n -> bar (Out n) x)

bar :: Storable b => Out Int -> In b -> IO [Word8]
bar (Out x) (In y) = do
    let sz = ptrSz y
    poke x sz
    peekArray sz (castPtr y)

baz :: Storable a => In a -> InOut a -> Out a -> IO ()
baz (In x) (InOut y) z = do
    let sz = ptrSz x
    
    memcpy z (In y) (fromIntegral sz)
    
    xBytes <- peekArray sz (castPtr x :: Ptr Word8)
    let yBytes = reverse xBytes
    pokeArray (castPtr y) yBytes
    
    return ()

qux :: In Word32 -> Out Word16 -> Out Word16 -> InOut Int -> IO Bool
qux (In x) (Out y) (Out z) counter = do
    [lo,hi] <- peekArray 2 (castPtr x :: Ptr Word16)
    
    poke y lo
    poke z hi
    
    n <- incr counter
    
    return (n `mod` 10 == 0)

quux :: Storable a
    =>  InOut Int -> In a -> In a -> Out a -> Out a -> IO Bool
quux ctr (In xIn) (In yIn) (Out xOut) (Out yOut) = do
    i <- incr ctr
    
    let swap = even i
    if swap
        then alloca $ \tmp -> do
            peek xIn >>= poke tmp
            peek yIn >>= poke xOut
            peek tmp >>= poke yOut
        else do
            peek xIn >>= poke xOut
            peek yIn >>= poke yOut
    
    return swap

-- and the same functions 'wrapped'.
-- These could easily be generated automatically, but ideally some type-hackery
-- would do it without the need for code generation.

foo' :: Storable a => a -> IO [Word8]
foo' = runReaderT (with foo)

bar' :: Storable b
     => b -> IO (Int,[Word8])
bar' b = do
    (c, First (Just a)) <- runReaderT (runWriterT (with2 bar)) b
    return (a,c)

baz' :: Storable a
     => a -> a -> IO (a,a)
baz' x y = do
    (((), y), First (Just z)) <- runWriterT (runStateT (runReaderT (with3 baz) x) y)
    return (y,z)

qux' :: Word32 -> Int -> IO (Word16, Word16, Int, Bool)
qux' a d = do
    (((e, First (Just b)), First (Just c)), d) <- runStateT (runWriterT (runWriterT (runReaderT (with4 qux) a))) d
    return (b,c,d,e)

quux' :: Storable a 
      => Int -> a -> a -> IO (Int, a, a, Bool)
quux' n x y = do
    (((swap, n), First (Just x)), First (Just y)) <- runWriterT (runWriterT (runReaderT (runReaderT (runStateT (with5 quux) n) x) y))
    return (n,x,y,swap)


-- some utility code
ptrSz :: Storable a => Ptr a -> Int
ptrSz = sizeOf . valType
    where
        valType :: Ptr a -> a
        valType = undefined

incr (InOut p) = do
    n <- peek p
    poke p (n+1)
    return n

-- 'In' and 'Out', etc., are valid FFI types too:
foreign import ccall "string.h memcpy"
    memcpy :: Out a -> In a -> CSize -> IO (Out a)

