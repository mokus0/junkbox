{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RecordWildCards #-}
module NR.Ch1.S4.IMat where

import NR.Ch1.S4.Linear
import NR.Ch1.S4.IVec

import Data.Permute
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV

class Linear m t => Matrix m t where
    matSize         :: m t -> (Int, Int)
    matRows         :: m t -> Int
    matRows = fst . matSize
    matCols         :: m t -> Int
    matCols = snd . matSize
    matrix          :: Int -> Int -> (Int -> Int -> t) -> m t
    unsafeIndexM    :: m t -> (Int -> Int -> t)




newtype ListMatrix a = ListMatrix [[a]]

instance Linear ListMatrix a where
    liftLinear f (ListMatrix m) = ListMatrix (map (map f) m)
    liftLinear2 f (ListMatrix m1) (ListMatrix m2) = ListMatrix (zipWith (zipWith f) m1 m2)
instance Matrix ListMatrix a where
    matSize (ListMatrix m) = (length m, maximum (map length m))
    matrix r c f = ListMatrix [[f i j | j <- [0..c-1]] | i <- [0..r-1]]
    unsafeIndexM (ListMatrix m) i j = m !! i !! j




data NRMatrix v a = NRMatrix 
    { nrMatRows :: !Int
    , nrMatCols :: !Int
    , nrMatVec  :: !(v a)
    } deriving (Eq, Show)

instance GV.Vector v a => Linear (NRMatrix v) a where
    liftLinear f NRMatrix{..} = NRMatrix{nrMatVec = GV.map f nrMatVec, ..}
    liftLinear2 f m1 m2 = NRMatrix
        { nrMatRows = min (nrMatRows m1) (nrMatRows m2)
        , nrMatCols = min (nrMatCols m1) (nrMatCols m2)
        , nrMatVec  = GV.zipWith f (nrMatVec m1) (nrMatVec m2)
        }
    
instance GV.Vector v a => Matrix (NRMatrix v) a where
    matSize NRMatrix{..} = (nrMatRows, nrMatCols)
    matRows = nrMatRows
    matCols = nrMatCols
    matrix r c f = NRMatrix r c (GV.generate n f')
        where
            n = r * c
            f' i = case i `divMod` c of
                (x,y) -> f x y
    unsafeIndexM NRMatrix{..} r c = nrMatVec GV.! (r * nrMatCols + c)

type IMatrix = NRMatrix V.Vector
type UMatrix = NRMatrix UV.Vector






-- unfiled
data FunctionMatrix t
    = FunctionMatrix
        { fmRows   :: !Int
        , fmCols   :: !Int
        , fmFunc   :: Int -> Int -> t
        }

instance Show t => Show (FunctionMatrix t) where
    showsPrec p = showsMatrix

instance Functor FunctionMatrix where
    fmap f (FunctionMatrix r c m) = FunctionMatrix r c (\i j -> f (m i j))
instance Linear FunctionMatrix t where
    liftLinear = fmap
    liftLinear2 f (FunctionMatrix r1 c1 m1) (FunctionMatrix r2 c2 m2) =
        FunctionMatrix (min r1 r2) (min r2 c2) (\i j -> f (m1 i j) (m2 i j))
instance Matrix FunctionMatrix t where
    matSize m = (fmRows m, fmCols m)
    matRows = fmRows
    matCols = fmCols
    matrix = FunctionMatrix
    unsafeIndexM = fmFunc

showMatrix mat = showsMatrix mat ""
showsMatrix mat = showString (unlines . map show . matrixToList $ mat)

{-# INLINE indexM #-}
indexM :: Matrix m t => m t -> Int -> Int -> t
indexM mat r c
    | r < 0             = error "matrix row index out of bounds"
    | r >= matRows mat  = error "matrix row index out of bounds"
    | c < 0             = error "matrix column index out of bounds"
    | c >= matCols mat  = error "matrix column index out of bounds"
    | otherwise         = unsafeIndexM mat r c

matrixToList :: Matrix m t => m t -> [[t]]
matrixToList m =
    [ [ indexM m i j
      | j <- [0..matCols m - 1]
      ]
    | i <- [0..matRows m - 1]
    ]

triDiag a b c = matrix n n f
    where
        n = minimum [length a, length b, length c]
        f i j = case j - i of
            -1  -> indexV a i
            0   -> indexV b i
            1   -> indexV c i
            _   -> 0

matrixFromList :: Matrix m t => [[t]] -> m t
matrixFromList m = matrix (length m) (minimum (map length m)) $ \r c -> m !! r !! c

imatrix :: Int -> Int -> (Int -> Int -> t) -> IMatrix t
imatrix = matrix

fmatrix :: Int -> Int -> (Int -> Int -> t) -> FunctionMatrix t
fmatrix = matrix

convertM :: (Matrix m1 a, Matrix m2 a) => m1 a -> m2 a
convertM = convertByM id
convertByM :: (Matrix m1 a, Matrix m2 b) => (a -> b) -> m1 a -> m2 b
convertByM f m = matrix (matRows m) (matCols m) (\i j -> f (unsafeIndexM m i j))

permuteRows :: Matrix m a => Permute -> m a -> FunctionMatrix a
permuteRows indx m = matrix (matRows m) (matCols m) (\i j -> indexM m (indx `at` i) j)

kronecker n = matrix n n $ \i j -> if i==j then 1 else 0

liftMatrix :: (Matrix m a, Matrix m b) => (a -> b) -> m a -> m b
liftMatrix = convertByM

liftMatrix2 :: (Matrix m a, Matrix m b, Matrix m c)
    => (a -> b -> c) -> m a -> m b -> m c
liftMatrix2 = convertByM2

convertByM2 (+) m1 m2 = matrix r c $ \i j -> indexM m1 i j + indexM m2 i j
    where
        r1 = matRows m1
        r | r1 == matRows m2    = r1
          | otherwise = error "liftMatrix2: matrix sizes don't match"
        c1 = matCols m1
        c | c1 == matCols m2    = c1
          | otherwise = error "liftMatrix2: matrix sizes don't match"
