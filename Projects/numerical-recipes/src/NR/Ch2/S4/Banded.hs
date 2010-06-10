module NR.Ch2.S4.Banded where

import NR.Ch1.S4
import Data.Permute
import Data.Permute.MPermute
import Data.StateRef
import Control.Monad
import Control.Monad.ST

-- test
-- import NR.Ch1.S4.Alias
-- a :: IMatrix Double
-- a  = matrixFromList [[1,3,-2,1],[3,5,6,2],[2,4,3,3],[1,2,7,4],[2,3,5,5]]
-- m1 = 1
-- m2 = 2
-- b :: IVector Double
-- b  = vectorFromList [5,7,8,2,19] 
-- a' :: IAlias Mat Double
-- a' = AsBandDiag m1 (IMat a) `Overlay` Fill 0
-- dcmp = bandec m1 m2 a
-- x :: IVector Double
-- x  = bandec_solve dcmp b
-- b' :: IVector Double
-- b' = a' `apply` x
-- b'' :: IVector Double
-- b'' = banmul m1 m2 a x


banmul m1 m2 a x = vector n $ \i -> sum
    [ indexM a i j * indexV x (j+k)
    | let k = i-m1
    , j <- [max 0 (negate k) .. min (m1+m2) (n-k-1)]
    ]
    
    where n = matRows a

data Bandec t = Bandec
    { bandec_n      :: Int
    , bandec_m1     :: Int
    , bandec_m2     :: Int
    , bandec_u      :: IMatrix t
    , bandec_l      :: IMatrix t
--    , bandec_indx   :: Permute
    , bandec_indx   :: IVector Int
    , bandec_d      :: Bool
    }

bandec m1 m2 a = runST (bandec_st m1 m2 a)
bandec_st m1 m2 a = do
    let n = matRows a
        mm = m1+m2+1
    
    au <- newMatrix_ n mm   :: ST s (STMatrix s Double) -- for now, generalize later
    al <- newMatrix_ n m1   :: ST s (STMatrix s Double) -- for now, generalize later
    repack a au m1 m2
    
    indx <- newVector_ n :: ST s (STVector s Int)
    d <- newRef True
    
    sequence_
        [ do
            let l = min n (m1 + k + 1)
            pivot au k l indx d
            
            -- eliminate
            sequence_
                [ do
                    au_i_0 <- readM au i 0
                    au_k_0 <- readM au k 0
                    let dum = au_i_0 / au_k_0
                    writeM al k (i-k-1) dum
                    
                    sequence_
                        [ do
                            au_i_j <- readM au i j
                            au_k_j <- readM au k j
                            writeM au i (j-1) (au_i_j - dum * au_k_j)
                        | j <- [1..mm-1]
                        ]
                    writeM au i (mm-1) 0
                | i <- [k+1 .. l-1]
                ]
        | k <- [0..n-1]
        ]
    
    au   <- unsafeFreezeMatrix au
    al   <- unsafeFreezeMatrix al
    indx <- unsafeFreezeVector indx
    d    <- readRef d
    return (Bandec n m1 m2 au al indx d)

bandec_solve (Bandec n m1 m2 au al indx d) b = runST $ do
    x <- copyVector b :: ST s (STVector s Double)
    let mm = m1+m2+1
    
    -- forward substitution
    sequence_
        [ do
            when (k /= k') $ swapVecElems x k k'
            
            x_k <- readV x k
            
            sequence_
                [ do
                    modifyV x j (subtract (indexM al k (j-k-1) * x_k))
                | j <- [k+1 .. l-1]
                ]
        | k <- [0 .. n-1]
        , let l = min (m1+k+1) n
              k' = indexV indx k
        ]
    
    -- back-substitution
    sequence_
        [ do
            updateV x i $ \x_i -> do
                dum <- let f dum k = do
                                x_kpi <- readV x (k+i)
                                return (dum - indexM au i k * x_kpi)
                         in foldM f x_i [1 .. l-1]
                
                return (dum / indexM au i 0)
        | i <- [n-1,n-2 .. 0]
        , let l = min (n-i) mm
        ]
    
    unsafeFreezeVector x

repack a au m1 m2 = do
    let n = matRows a
    l <- newRef m1
    sequence_
        [ do
            let x   | j <= m2+m1 - l
                    = indexM a i (j+l)
                    | otherwise = 0
            writeM au i j x
        | i <- [0..n-1]
        , let l = max (m1-i) 0
        , j <- [0 .. m1+m2]
        ]

pivot au k l indx d = do
    i <- selectPivot au k l
    
    writeV indx k i
    when (i /= k) $ do
        modifyRef d not
        swapRowsM au   i k

selectPivot au k l = go [k .. l-1] k 0
    where
        go [] i 0 = fail "bandec: algorithmically singular matrix"
        go [] i _ = return i
        go (j:js) i best = do
            au_j_0 <- readM au j 0
            if abs au_j_0 > abs best
                then go js j au_j_0
                else go js i best
    