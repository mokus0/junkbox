{-# LANGUAGE GADTs, TypeFamilies, EmptyDataDecls #-}
module TypeExperiments.Transactional where

import Data.IORef

class Transactional a where
    data Transaction a :: * -> * -> *
    transact :: Transaction a b c -> b -> c

instance Transactional () where
    data Transaction () a b where
        Anonymize :: Transactional t => Transaction t a b -> Transaction () a b
        Id :: Transaction () a a
        Compose :: (Transactional x, Transactional y) => Transaction x b c -> Transaction y a b -> Transaction () a c
        Const :: b -> Transaction () a b
    transact (Anonymize t) = transact t
    transact Id = id
    transact (Compose tx ty) = transact tx . transact ty
    transact (Const k) = const k

instance Transactional Int where
    data Transaction Int a b where
        Add         :: Transaction Int (Int,Int) Int
        Mul         :: Transaction Int (Int,Int) Int
    transact Add = uncurry (+)
    transact Mul = uncurry (*)

data Pair
instance Transactional Pair where
    data Transaction Pair c d where
        Fst   :: Transaction Pair (a,b) a
        Snd   :: Transaction Pair (a,b) b
        Pair  :: (Transactional x, Transactional y) => Transaction x t a -> Transaction y t b -> Transaction Pair t (a,b)
    transact Fst (x, y) = x
    transact Snd (x, y) = y
    transact (Pair tx ty) z = (transact tx z, transact ty z)

onFst t = Pair (Compose t Fst) Snd
onSnd t = Pair Fst (Compose t Snd)
swap    = Pair Snd Fst
curryT t a b = t (Pair a b)
dup :: Transaction Pair a (a,a)
dup = Pair Id Id

instance Transactional a => Transactional (IORef a) where
    data Transaction (IORef a) b c where
        InPlace :: Transaction a b b -> Transaction (IORef a) (IORef b) (IO ())
        InPlace2 :: Transaction a (b,c) (b,c) -> Transaction (IORef a) (IORef b, IORef c) (IO ())
    transact (InPlace tx) ref = do
        x <- readIORef ref
        writeIORef ref (transact tx x)
    transact (InPlace2 tx) (rx, ry) = do
        x <- readIORef rx
        y <- readIORef ry
        let (x', y') = transact tx (x,y)
        writeIORef rx x'
        writeIORef ry y'
