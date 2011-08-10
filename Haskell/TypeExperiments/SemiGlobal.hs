{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |Semi-global variables.  This is an ST-like monad transformer which 
-- implements "variables" which are declared purely (using a statically-
-- provided default value) and can be used in a "global" style within the
-- scope, but only within the scope of the 'runSGT' invocation.
module TypeExperiments.SemiGlobal
    ( SG, SGT, runSG, runSGT
    , SGVar, newSGVar, readSGVar, writeSGVar, resetSGVar
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.Dependent.Map as M
import Data.Maybe
import Data.GADT.Compare
import Data.Typeable

type SG = SGT Identity
runSG :: SG a -> a
runSG = runIdentity . runSGT

newtype SGT m a = SGT { unSG :: StateT (DMap SGVar) m a}
    deriving (Functor, Applicative, Monad)

runSGT :: Monad m => SGT m a -> m a
runSGT (SGT x) = evalStateT x M.empty

data SGVar t where
    SGVar :: (Typeable1 f, GCompare f) => !(f t) -> !t -> SGVar t

cast1 :: (Typeable1 f, Typeable1 g) => f a -> Maybe (g a)
cast1 = fmap runIdentity . gcast1 . Identity

instance GEq SGVar where
    geq (SGVar t1 _) (SGVar t2 _) = do
        t2 <- cast1 t2
        geq t1 t2

instance GCompare SGVar where
    gcompare (SGVar t1 _) (SGVar t2 _) = case cast1 t2 of
        Nothing -> case compare (show (typeOf1 t1)) (show (typeOf1 t2)) of
            LT -> GLT
            GT -> GGT
            EQ -> error "gcompare@SGVar: typereps have same string representation but are not (==)"
        Just t2 -> gcompare t1 t2
        
newSGVar :: (Typeable1 f, GCompare f) => f t -> t -> SGVar t
newSGVar = SGVar

readSGVar :: Monad m => SGVar t -> SGT m t
readSGVar v@(SGVar _ defVal) 
    = liftM (fromMaybe defVal)
    $ SGT (gets (M.lookup v))

writeSGVar :: Monad m => SGVar t -> t -> SGT m ()
writeSGVar v x
    = SGT (modify (M.insert v x))

resetSGVar :: Monad m => SGVar t -> SGT m ()
resetSGVar v
    = SGT (modify (M.delete v))
