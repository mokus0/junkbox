module TypeExperiments.PrimRef
    ( Ref
    , newRef
    , readRef
    , writeRef
    , modifyRef
    ) where

import Control.Applicative
import Control.Monad.Primitive
import Control.Monad.ST
import Data.STRef

newtype Ref m a = Ref (STRef (PrimState m) a)
    deriving (Eq)

newRef :: PrimMonad m => a -> m (Ref m a)
newRef x = primToPrim (Ref <$> newSTRef x)

readRef :: PrimMonad m => Ref m a -> m a
readRef (Ref r) = primToPrim (readSTRef r)

writeRef :: PrimMonad m => Ref m a -> a -> m ()
writeRef (Ref r) = primToPrim . writeSTRef r

modifyRef :: PrimMonad m => Ref m a -> (a -> a) -> m ()
modifyRef (Ref r) = primToPrim . modifySTRef r
