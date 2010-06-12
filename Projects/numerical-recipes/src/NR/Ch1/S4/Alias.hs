{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GADTs, EmptyDataDecls #-}
module NR.Ch1.S4.Alias where

import NR.Ch1.S4

import Control.Monad.Identity
import Data.Permute
import Data.Permute.MPermute
import Data.Monoid

data Mat
data Vec

type IAlias k = Alias k Identity
data Alias k m t where
    FMap        :: (a -> b) -> Alias k m a -> Alias k m b
    ZipWith     :: (a -> b -> c) -> Alias k m a -> Alias k m b -> Alias k m c
    MMat        :: MMatrix mat t m => mat t -> Alias Mat m t
    MVec        :: MVector vec t m => vec t -> Alias Vec m t
    IMat        :: Matrix mat t    => mat t -> Alias Mat m t
    IVec        :: Vector vec t    => vec t -> Alias Vec m t
    AsDiag      :: Alias Vec m t -> t -> Alias Mat m t
    AsRow       :: Alias Vec m t      -> Alias Mat m t
    AsCol       :: Alias Vec m t      -> Alias Mat m t
    Transpose   :: Alias Mat m t      -> Alias Mat m t
    Diag        :: Alias Mat m t      -> Alias Vec m t
    Row         :: Int -> Alias Mat m t -> Alias Vec m t
    Col         :: Int -> Alias Mat m t -> Alias Vec m t
    AsBandDiag  :: Int        -> Alias Mat m t -> Alias Mat m t
    PackBandDiag:: Int -> Int -> Alias Mat m t -> Alias Mat m t
    ShiftM      :: Int -> Int -> Alias Mat m t -> Alias Mat m t
    CropM       :: Int -> Int -> Alias Mat m t -> Alias Mat m t
    ShiftV      :: Int        -> Alias Vec m t -> Alias Vec m t
    CropV       :: Int        -> Alias Vec m t -> Alias Vec m t
    RowPermute  :: Permute -> Alias Mat m t -> Alias Mat m t
    ColPermute  :: Permute -> Alias Mat m t -> Alias Mat m t
    VecPermute  :: Permute -> Alias Vec m t -> Alias Vec m t
    RowMPermute :: MPermute p m => p -> Alias Mat m t -> Alias Mat m t
    ColMPermute :: MPermute p m => p -> Alias Mat m t -> Alias Mat m t
    VecMPermute :: MPermute p m => p -> Alias Vec m t -> Alias Vec m t
    Overlay     :: Alias k m t -> Alias k m t -> Alias k m t
    Fill        :: t -> Alias k m t

{-# RULES 
"Flatten Alias IVec"    forall v. IVec v = v
"Flatten Alias IMat"    forall m. IMat m = m
"Flatten Alias MVec"    forall v. MVec v = v
"Flatten Alias MMat"    forall m. MMat m = m
"Alias FMap/ZipWith"    forall f g m1 m2. FMap f (ZipWith g m1 m2) = ZipWith (\x y -> f (g x y)) m1 m2
"Alias FMap/ZipWith"    forall f g m1 m2. ZipWith f (FMap g m1) m2 = ZipWith (\x y -> f (g x) y) m1 m2
"Alias FMap/ZipWith"    forall f g m1 m2. ZipWith f m1 (FMap g m2) = ZipWith (\x y -> f x (g y)) m1 m2
  #-}

type ICellAlias = CellAlias Identity
data CellAlias m t where
    FMapCell    :: (a -> b) -> CellAlias m a -> CellAlias m b
    ZipWithCell :: (a -> b -> c) -> CellAlias m a -> CellAlias m b -> CellAlias m c
    MMatCell    :: MMatrix mat t m => Int -> Int -> mat t -> CellAlias m t
    MVecCell    :: MVector vec t m => Int ->        vec t -> CellAlias m t
    ConstCell   :: t -> CellAlias m t
    ROConstCell :: t -> CellAlias m t
    NoCell      :: CellAlias m t

readICellM :: IAlias Mat t -> Int -> Int -> t
readICellM m i j = runIdentity (lookupAliasM m i j >>= readCell)

readICellV :: IAlias Vec t -> Int -> t
readICellV v i = runIdentity (lookupAliasV v i >>= readCell)

readCell :: Monad m => CellAlias m t -> m t
readCell (FMapCell f c)     = do
    x <- readCell c
    return (f x)
readCell (ZipWithCell f a b)     = do
    x <- readCell a
    y <- readCell b
    return (f x y)
readCell (MMatCell i j m)   = readM m i j
readCell (MVecCell i v)     = readV v i
readCell (ConstCell   t)    = return t
readCell (ROConstCell t)    = return t
readCell NoCell = fail "Alias.readCell: cell read was not aliased to anything"

writeCell :: Monad m => CellAlias m t -> t -> m ()
writeCell (MMatCell i j m)  x = writeM m i j x
writeCell (MVecCell i v)    x = writeV v i x
writeCell (ROConstCell t)   x = fail "Alias.writeCell: cell written is read-only"
writeCell (ConstCell   t)   x = return ()
writeCell NoCell x = fail "Alias.writeCell: cell written was not aliased to anything"

inBndsM i j (r,c) = and
    [ i >= 0
    , j >= 0
    , i < r
    , j < c
    ]

lookupAliasM :: Monad m => Alias Mat m t -> Int -> Int -> m (CellAlias m t)
lookupAliasM (FMap f m) i j = do
    c <- lookupAliasM m i j
    return (FMapCell f c)
lookupAliasM (ZipWith f m1 m2) i j = do
    c1 <- lookupAliasM m1 i j
    c2 <- lookupAliasM m2 i j
    return (ZipWithCell f c1 c2)
lookupAliasM (IMat m) i j = do
    let sz = matSize m
    if inBndsM i j sz
        then return (ROConstCell (indexM m i j))
        else return NoCell
lookupAliasM (MMat m) i j = do
    sz <- getMatSize m
    if inBndsM i j sz
        then return (MMatCell i j m)
        else return NoCell
lookupAliasM (AsDiag v z) i j
    | i == j    = lookupAliasV v i
    | otherwise = return (ConstCell z)
lookupAliasM (AsRow v)        0 j = lookupAliasV v j
lookupAliasM (AsCol v)        i 0 = lookupAliasV v i
lookupAliasM (Transpose m)    i j = lookupAliasM m j i
lookupAliasM (ShiftM di dj m) i j = lookupAliasM m (i-di) (j-dj)
lookupAliasM (AsBandDiag dj m)   i j = do
    r <- getNumRows m
    if i < r && j < r
        then lookupAliasM m i (j-i+dj)
        else return NoCell
lookupAliasM (PackBandDiag sub sup m) i j = do
    let k = i + j - sub
    if j >= 0 && j <= sub+sup
        then lookupAliasM m i k
        else return NoCell
lookupAliasM (CropM r c m)    i j
    | inBndsM i j (r,c) = lookupAliasM m i j
    | otherwise = return NoCell
lookupAliasM (Overlay m1 m2)  i j = do
    c1 <- lookupAliasM m1 i j
    case c1 of
        NoCell  ->  lookupAliasM m2 i j
        _       ->  return c1
lookupAliasM (RowPermute p m) i j = lookupAliasM m (p `at` i) j
lookupAliasM (ColPermute p m) i j = lookupAliasM m i (p `at` j)
lookupAliasM (RowMPermute p m) i j = do
    i <- getElem p i
    lookupAliasM m i j
lookupAliasM (ColMPermute p m) i j = do
    j <- getElem p j
    lookupAliasM m i j
lookupAliasM (Fill t) i j         = return (ConstCell t)
lookupAliasM _ _ _ = return NoCell

aliasSizeM :: Monad m => Alias Mat m t -> m (Int, Int)
aliasSizeM (FMap f m) = aliasSizeM m
aliasSizeM (ZipWith f m1 m2) = do
    (r1,c1) <- aliasSizeM m1
    (r2,c2) <- aliasSizeM m2
    return (min r1 r2, min c1 c2)
aliasSizeM (IMat m) = return (matSize m)
aliasSizeM (MMat m) = getMatSize m
aliasSizeM (AsDiag v z) = do
    i <- aliasSizeV v
    return (i,i)
aliasSizeM (AsRow v) = do
    i <- aliasSizeV v
    return (1,i)
aliasSizeM (AsCol v) = do
    i <- aliasSizeV v
    return (i,1)
aliasSizeM (RowPermute p m) = aliasSizeM m
aliasSizeM (ColPermute p m) = aliasSizeM m
aliasSizeM (RowMPermute p m) = aliasSizeM m
aliasSizeM (ColMPermute p m) = aliasSizeM m
aliasSizeM (AsBandDiag n m) = do
    (r,c) <- aliasSizeM m
    return (r,r)
aliasSizeM (PackBandDiag sub sup m) = do
    (r,c) <- aliasSizeM m
    return (r, 1+sub+sup)
aliasSizeM (ShiftM di dj m) = do
    (r,c) <- aliasSizeM m
    return (r+di, c+dj)
aliasSizeM (CropM r c m) = return (r,c)
aliasSizeM (Overlay m1 m2) = do
    (r1,c1) <- aliasSizeM m1
    (r2,c2) <- aliasSizeM m2
    return (max r1 r2, max c1 c2)
aliasSizeM (Fill t) = return (0,0)

inBndsV i n = (i >= 0) && (i < n)

lookupAliasV :: Monad m => Alias Vec m t -> Int -> m (CellAlias m t)
lookupAliasV (FMap f v) i = do
    c <- lookupAliasV v i
    return (FMapCell f c)
lookupAliasV (ZipWith f v1 v2) i = do
    c1 <- lookupAliasV v1 i
    c2 <- lookupAliasV v2 i
    return (ZipWithCell f c1 c2)
lookupAliasV (IVec v) i = do
    let n = vecElems v
    if inBndsV i n
        then return (ROConstCell (indexV v i))
        else return NoCell
lookupAliasV (MVec v) i = do
    n <- getVecSize v
    if inBndsV i n
        then return (MVecCell i v)
        else return NoCell
lookupAliasV (Row i m) j = lookupAliasM m i j
lookupAliasV (Col j m) i = lookupAliasM m i j
lookupAliasV (Diag m)  i = lookupAliasM m i i

lookupAliasV (ShiftV di v)   i = lookupAliasV v (i-di)
lookupAliasV (CropV  n v)    i 
    | inBndsV i n = lookupAliasV v i
    | otherwise = return NoCell
lookupAliasV (Overlay v1 v2) i = do
    c1 <- lookupAliasV v1 i
    case c1 of
        NoCell  -> lookupAliasV v2 i
        _       -> return c1
lookupAliasV (VecPermute p v) i = lookupAliasV v (p `at` i)
lookupAliasV (VecMPermute p v) i = do
    i <- getElem p i
    lookupAliasV v i
lookupAliasV (Fill t) i = return (ConstCell t)
lookupAliasV _ _ = return NoCell

aliasSizeV :: Monad m => Alias Vec m t -> m Int
aliasSizeV (FMap f v) = aliasSizeV v
aliasSizeV (ZipWith f v1 v2) = do
    n1 <- aliasSizeV v1
    n2 <- aliasSizeV v2
    return (min n1 n2)
aliasSizeV (IVec m) = return (vecElems m)
aliasSizeV (MVec m) = getVecSize m
aliasSizeV (VecPermute p m) = aliasSizeV m
aliasSizeV (VecMPermute p m) = aliasSizeV m
aliasSizeV (Row r m) = do
    (_,c) <- aliasSizeM m
    return c
aliasSizeV _ = error "aliasSizeV: not completely implemented"


-- doesn't need to use 'return', but it allows the
-- alias to be automagically linked to the type of the
-- Monad in use.
aliasMatrixWith :: (Monad m, MMatrix mat t m) =>
                   (Alias Mat m t -> a) -> mat t -> m a
aliasMatrixWith f m = return (f (MMat m))

instance Matrix (Alias Mat Identity) t where
    matSize = runIdentity . aliasSizeM
    matrix r c m = (IMat :: FunctionMatrix a -> IAlias Mat a) (matrix r c m)
    unsafeIndexM = readICellM
    
instance Monad m => MMatrix (Alias Mat m) t m where
    newMatrix_ r c = error "newMatrix_ doesn't make sense for type 'Alias Mat'"
    newMatrix r c m = return ((IMat :: FunctionMatrix a -> Alias Mat m a) (matrix r c m))
    readM m i j = do
        cell <- lookupAliasM m i j
        readCell cell
        
    writeM m i j x = do
        cell <- lookupAliasM m i j
        writeCell cell x
        
    getMatSize = aliasSizeM

aliasVectorWith :: (Monad m, MVector vec t m) =>
                   (Alias Vec m t -> a) -> vec t -> m a
aliasVectorWith f v = return (f (MVec v))

aliasRow, aliasCol :: MMatrix mat t m => mat t -> Int -> m (Alias Vec m t)
aliasRow m i = return (Row i (MMat m))
aliasCol m j = return (Row j (MMat m))

row :: (Matrix mat a) => Int -> mat a -> IAlias Vec a
row n mat = Row n (IMat mat)

col :: (Matrix mat a) => Int -> mat a -> IAlias Vec a
col n mat = Col n (IMat mat)

instance Functor (Alias k m) where
    fmap = FMap
instance Linear (Alias k m) t where
    liftLinear = FMap
    liftLinear2 = ZipWith
instance Vector (Alias Vec Identity) t where
    vecElems = runIdentity . aliasSizeV
    vector n v = (IVec :: FunctionVector a -> IAlias Vec a) (vector n v)
    unsafeIndexV = readICellV

instance Monad m => MVector (Alias Vec m) t m where
    newVector_ n   = error "newVector_ doesn't make sense for type 'Alias Vec'"
    newVector  n v = error "newVector doesn't make sense for type 'Alias Vec'"
    readV v i = do
        cell <- lookupAliasV v i
        readCell cell
        
    writeV v i x = do
        cell <- lookupAliasV v i
        writeCell cell x
        
    getVecSize = aliasSizeV

