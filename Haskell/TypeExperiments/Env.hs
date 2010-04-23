{-# LANGUAGE GADTs #-}
module TypeExperiments.Env where

import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.Dynamic
import TypeExperiments.Uniq
import Control.Monad.Primitive
import TypeExperiments.Dependent

newtype Env s = Env (M.Map (Uniq s) Dynamic)
data EnvKey s a where
    EnvKey :: Typeable a => !(Uniq s) -> EnvKey s a

empty :: Env s
empty = Env M.empty

singleton :: Typeable a => EnvKey s a -> a -> Env s
singleton key thing = insert key thing empty

null :: Env s -> Bool
null (Env m) = M.null m

size :: Env s -> Int
size (Env m) = M.size m

newKey :: (PrimMonad m, Typeable a) => m (EnvKey (PrimState m) a)
newKey = do
    u <- getUniq
    return (EnvKey u)

fromList :: [DSum (EnvKey s)] -> Env s
fromList [] = empty
fromList (DSum k v : rest) = insert k v (fromList rest)

insert :: EnvKey s a -> a -> Env s -> Env s
insert (EnvKey u) thing (Env m) = Env (M.insert u (toDyn thing) m)

insertWith :: (a -> a -> a) -> EnvKey s a -> a -> Env s -> Env s
insertWith f (EnvKey u) thing (Env m) = Env (M.insertWith f' u (toDyn thing) m)
    where
        e = error "Env.insertWith: key not present or type error"
        f' x y = toDyn (f (fromDyn x e) (fromDyn y e))

lookup :: EnvKey s a -> Env s -> Maybe a
lookup (EnvKey u) (Env m) = do
    dyn <- M.lookup u m
    fromDynamic dyn

delete :: EnvKey s a -> Env s -> Env s
delete (EnvKey u) (Env m) = Env (M.delete u m)


(!) :: Env s -> EnvKey s a -> a
Env m ! EnvKey u = fromDyn (m M.! u) $
    error "Env.!: key not present or type error"

