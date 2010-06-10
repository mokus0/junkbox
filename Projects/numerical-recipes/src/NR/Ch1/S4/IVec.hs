{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module NR.Ch1.S4.IVec where

import NR.Ch1.S4.Linear

import Data.Permute
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV

class Linear v t => Vector v t where
    vecElems        :: v t -> Int
    vector          :: Int -> (Int -> t) -> v t
    unsafeIndexV    :: v t -> (Int -> t)

showVector vec = showsVector vec ""
showsVector vec = showString (unlines . map show . vectorToList $ vec)

{-# INLINE indexV #-}
indexV :: Vector v t => v t -> Int -> t
indexV vec i
    | i < 0             = error "vector index out of bounds"
    | i >= vecElems vec = error "vector index out of bounds"
    | otherwise         = unsafeIndexV vec i

vectorFromList :: Vector v t => [t] -> v t
vectorFromList v = vector (length v) (v!!)

vectorToList :: Vector v t => v t -> [t]
vectorToList v =
    [ indexV v i
    | i <- [0..vecElems v - 1]
    ]



instance Vector [] t
    where
        vector n v = [v i | i <- [0..n-1]]
        vecElems = length
        unsafeIndexV = (!!)



newtype NRVector v a = NRVector (v a)

instance GV.Vector v a => Linear (NRVector v) a where
    liftLinear f (NRVector v) = NRVector (GV.map f v)
    liftLinear2 f (NRVector v1) (NRVector v2) = NRVector (GV.zipWith f v1 v2)
    
instance GV.Vector v a => Vector (NRVector v) a where
    vecElems (NRVector v) = GV.length v
    vector n f = NRVector (GV.generate n f)
    unsafeIndexV (NRVector v) i = v GV.! i

type IVector = NRVector V.Vector
type UVector = NRVector UV.Vector

ivector :: Int -> (Int -> t) -> IVector t
ivector = vector
uvector :: GV.Vector UV.Vector t => Int -> (Int -> t) -> UVector t
uvector = vector



data FunctionVector t
    = FunctionVector
        { fvSize    :: !Int
        , fvFunc    :: Int -> t
        }

instance Show t => Show (FunctionVector t)
    where
        showsPrec p = showsVector

instance Functor FunctionVector
    where
        fmap f (FunctionVector n v) = FunctionVector n (f.v)
instance Linear FunctionVector t
    where
        liftLinear = fmap
        liftLinear2 = liftVector2
instance Vector FunctionVector t
    where
        vecElems = fvSize
        vector = FunctionVector
        unsafeIndexV = fvFunc

fvector :: Int -> (Int -> t) -> FunctionVector t
fvector = vector






-- unfiled
convertV :: (Vector v1 a, Vector v2 a) => v1 a -> v2 a
convertV = convertByV id
convertByV :: (Vector v1 a, Vector v2 b) => (a -> b) -> v1 a -> v2 b
convertByV f v = vector (vecElems v) (\i -> f (unsafeIndexV v i))
permuteV :: Vector v a => Permute -> v a -> FunctionVector a
permuteV indx v = vector (vecElems v) (indexV v . at indx)
liftVector :: (Vector v a, Vector v b) => (a -> b) -> v a -> v b
liftVector = convertByV
liftVector2 :: (Vector v a, Vector v b, Vector v c)
    => (a -> b -> c) -> v a -> v b -> v c
liftVector2 = convertByV2
convertByV2 (+) v1 v2 = vector n $ \i -> indexV v1 i + indexV v2 i
    where
        n1 = vecElems v1
        n | n1 == vecElems v2 = n1
          | otherwise = error "liftVector2: vector sizes don't match"
