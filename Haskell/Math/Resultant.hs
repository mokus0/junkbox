module Math.Resultant where

import Math.Polynomial
import Data.Matrix.Types
import Data.Matrix.Algorithms.LUDecomp (det)

res p q = naive_det (sylvester p q)

sylvester p q = imatrix sz sz f
    where
        sz = n + m
        m = polyDegree p
        n = polyDegree q
        
        pCoeff i
            | j < 0 || j > m    = 0
            | otherwise         = polyCoeffs BE p !! j
            where j = i --m --  - i -- + m
        
        qCoeff i
            | j < 0 || j > n    = 0
            | otherwise         = polyCoeffs BE q !! j
            where j = i-- m -- i  -- n - 1
        
        f i j
            | i' < 0    = pCoeff (j - i)
            | otherwise = qCoeff (j - i')
                where i' = i - n

naive_det m
    | uncurry (/=) (matSize m)
    = error "naive_det: matrix is not square"
    
    | sz == 1   = indexM m 0 0
    
    | otherwise
    = sum [sgn * indexM m 0 i * naive_det (clear i) | i <- [0..sz-1], let sgn = if even i then 1 else -1 ]
    
    where
        sz = matRows m
        
        clear c = imatrix (sz - 1) (sz - 1) $ \i j ->
            indexM m (i+1) (if j < c then j else j + 1)
    