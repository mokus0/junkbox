module TypeExperiments.PrimRef
    ( Ref
    , newRef
    , readRef
    , writeRef
    , modifyRef
    
    , refToSTRef
    , refFromSTRef
    
    , STRef
    , newSTRef
    , readSTRef
    , writeSTRef
    , modifySTRef
    ) where

import Control.Applicative
import Control.Monad.Primitive
import qualified Control.Monad.ST as ST
import Data.STRef (STRef)
import qualified Data.STRef       as ST

newtype Ref m a = Ref (ST.STRef (PrimState m) a)
    deriving (Eq)

newRef :: PrimMonad m => a -> m (Ref m a)
newRef x = primToPrim (Ref <$> ST.newSTRef x)

readRef :: PrimMonad m => Ref m a -> m a
readRef (Ref r) = readSTRef r

writeRef :: PrimMonad m => Ref m a -> a -> m ()
writeRef (Ref r) = writeSTRef r

modifyRef :: PrimMonad m => Ref m a -> (a -> a) -> m ()
modifyRef (Ref r) = modifySTRef r


refToSTRef :: Ref m a -> STRef (PrimState m) a
refToSTRef (Ref r) = r

refFromSTRef :: STRef (PrimState m) a -> Ref m a
refFromSTRef = Ref


-- similar case with less-constrained reference type (uses 'STRef' as-is,
-- parameterized over state rather than monad, so different monads with same
-- state-thread can share refs)
newSTRef :: PrimMonad m => a -> m (STRef (PrimState m) a)
newSTRef x = primToPrim (ST.newSTRef x)

readSTRef :: PrimMonad m => STRef (PrimState m) a -> m a
readSTRef r = primToPrim (ST.readSTRef r)

writeSTRef :: PrimMonad m => STRef (PrimState m) a -> a -> m ()
writeSTRef r x = primToPrim (ST.writeSTRef r x)

modifySTRef :: PrimMonad m => STRef (PrimState m) a -> (a -> a) -> m ()
modifySTRef r f = primToPrim (ST.modifySTRef r f)