{-# LANGUAGE GADTs #-}
module TypeExperiments.Env where

import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.Dynamic
import TypeExperiments.Uniq
import Control.Monad.Primitive
import TypeExperiments.Dependent

-- These must both mention the 'state' token, so that the following expressions
-- will not be typeable (as they are not referentially transparent):
--
--  >   foo = runST newKey
--  >   bar = runST $ do {k <- newKey; return (singleton k thing)}
-- 
-- The former shows that 'EnvKey' must be tagged.
-- The latter may seem 'safe', as long as the key can never be 
-- extracted and the order of the keys is never exposed.  However, consider:
--  >   qux = union bar bar
--
-- The value of 'size qux' then depends on whether or not the
-- computation of bar was shared between its two references.  Therefore
-- 'Env' must be tagged as well.

newtype Env s = Env (M.Map (Uniq s) Dynamic)
data EnvKey s a where
    EnvKey :: Typeable a => !(Uniq s) -> EnvKey s a

-- Internal: just a standard error message indicating an indexing error.
envKeyErr str = error ("Env." ++ str ++ ": key not present or type error")

-- This is the only place the Typeable context is mentioned: the Typeable 
-- dictionary is captured and stored in the key.  This is also the only way in
-- which an 'Env' is tied to a monad.
newKey :: (PrimMonad m, Typeable a) => m (EnvKey (PrimState m) a)
newKey = do
    u <- getUniq
    return (EnvKey u)

empty :: Env s
empty = Env M.empty

singleton :: EnvKey s a -> a -> Env s
singleton key thing = insert key thing empty

null :: Env s -> Bool
null (Env m) = M.null m

size :: Env s -> Int
size (Env m) = M.size m

fromList :: [DSum (EnvKey s)] -> Env s
fromList [] = empty
fromList (DSum k v : rest) = insert k v (fromList rest)

member :: EnvKey s a -> Env s -> Bool
member (EnvKey k) (Env m) = M.member k m

insert :: EnvKey s a -> a -> Env s -> Env s
insert (EnvKey u) thing (Env m) = Env (M.insert u (toDyn thing) m)

insertWith :: (a -> a -> a) -> EnvKey s a -> a -> Env s -> Env s
insertWith f (EnvKey u) thing (Env m) = Env (M.insertWith f' u (toDyn thing) m)
    where
        e = envKeyErr "insertWith"
        f' x y = toDyn (f (fromDyn x e) (fromDyn y e))

lookup :: EnvKey s a -> Env s -> Maybe a
lookup (EnvKey u) (Env m) = do
    dyn <- M.lookup u m
    fromDynamic dyn

delete :: EnvKey s a -> Env s -> Env s
delete (EnvKey u) (Env m) = Env (M.delete u m)

(!) :: Env s -> EnvKey s a -> a
Env m ! EnvKey u = fromDyn (m M.! u) $ envKeyErr "!"

(\\) :: Env s -> Env s -> Env s
Env m1 \\ Env m2 = Env (m1 M.\\ m2)

intersection :: Env s -> Env s -> Env s
intersection (Env m1) (Env m2) = Env (M.intersection m1 m2)

difference :: Env s -> Env s -> Env s
difference (Env m1) (Env m2) = Env (M.difference m1 m2)

union :: Env s -> Env s -> Env s
union (Env m1) (Env m2) = Env (M.union m1 m2)

unions :: [Env s] -> Env s
unions envs = Env $ M.unions [ m | Env m <- envs]

update :: (a -> Maybe a) -> EnvKey s a -> Env s -> Env s
update f (EnvKey k) (Env m) = Env (M.update f' k m)
    where
        f' = fmap toDyn . f . flip fromDyn (envKeyErr "update")
