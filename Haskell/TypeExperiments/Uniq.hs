module TypeExperiments.Uniq
    (Uniq, newUniq) where

import Control.Monad.Primitive
import TypeExperiments.PrimRef

newtype Uniq s = Uniq (STRef s (Uniq s))
    deriving Eq

newUniq :: PrimMonad m => m (Uniq (PrimState m))
newUniq = do
    ref <- newSTRef undefined
    let uniq = Uniq ref
    writeSTRef ref uniq
    return uniq
