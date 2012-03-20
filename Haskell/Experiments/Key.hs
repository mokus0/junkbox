-- Simple implementation of interface described at
-- http://apfelmus.nfshost.com/blog/2011/09/04-vault.html
-- using dependent-map and IORef-based keys instead of Data.Unique.Tag.
{-# LANGUAGE GADTs #-}
module Experiments.Key where

import Data.Dependent.Sum
import Data.GADT.Compare
import Data.GADT.Compare
import Data.IORef
import qualified Data.Dependent.Map as M;
import Data.Unique

data Key a = Key !Unique !(IORef (Maybe a))

data KeyRec a where
    KeyRec :: !(Key a) -> KeyRec (IO ())

instance GEq KeyRec where
    geq (KeyRec (Key u1 _)) (KeyRec (Key u2 _))
        | u1 == u2  = Just Refl
        | otherwise = Nothing

instance GCompare KeyRec where
    gcompare (KeyRec (Key u1 _)) (KeyRec (Key u2 _)) = case compare u1 u2 of
        LT -> GLT
        EQ -> GEQ
        GT -> GGT

type Vault = M.DMap KeyRec

insert :: Key a -> a -> Vault -> Vault
insert k@(Key _ r) v = M.insert (KeyRec k) (writeIORef r (Just v))

lookup :: Key a -> Vault -> IO (Maybe a)
lookup k@(Key _ r) v = case M.lookup (KeyRec k) v of
    Just it -> do
        writeIORef r Nothing
        it
        readIORef r
    Nothing -> return Nothing

newKey :: IO (Key a)
newKey = do
    k   <- newUnique
    ref <- newIORef Nothing
    return $ Key k ref
