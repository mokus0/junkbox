{-
 - Golub-Kahan-Lanczos bidiagonalization, translated from pseudocode given
 - at http://www.cs.utk.edu/~dongarra/etemplates/node198.html
 - (as accessed 11 Aug 2009)
 -
 - very crudely coded right now...
 -}
module Math.Bidiagonalization where

import NR.Ch1.S4
import Data.Complex
import Data.List
import Data.Ord

-- |Given a square matrix A, compute square matrices U and V and vectors Alpha 
-- and Beta such that U*AV is bidiagonal with Alpha on the diagonal
-- and Beta above the diagonal.
bidiagonalize_r a = bidiagonalize_g (comparing abs) id a
bidiagonalize_c a = bidiagonalize_g (comparing magnitude) conjugate a

bidiagonalize_g :: (Floating t, Matrix m t) =>
                   (t -> t -> Ordering)
                   -> (t -> t)
                   -> m t
                   -> (IMatrix t, IMatrix t,
                       IVector t, IVector t)
bidiagonalize_g cmp conj a = (u,v,alpha,beta)
    where
        mat :: (Int -> Int -> t) -> IMatrix t
        mat = matrix n n
        vec :: (Int -> t) -> IVector t
        vec = vector n
        
        n = matRows a
        u'    = mat $ \i k -> if k == 0 then av i k else av i k - indexV beta (k-1) * indexM u i (k-1)
        alpha = vec $ \k   -> pythag [indexM u' j k | j <- [0..n-1]]
        u     = mat $ \i k -> indexM u' i k / indexV alpha k
        v'    = mat $ \i k -> aStarU i (k-1) - indexV alpha (k-1) * indexM v i (k-1)
        beta  = vec $ \k   -> if k == n-1 then 0    else pythag [indexM v' j (k+1) | j <- [0..n-1]]
        v     = mat $ \i k -> if k == 0   then v0 i else indexM v' i k / indexV beta (k-1)
        
        v0 0 = 1
        v0 _ = 0
        
        av     i k = foldl1' (+) [unsafeIndexM a i j * unsafeIndexM v j k | j <- [0..n - 1]]
        aStarU i k = foldl1' (+) [conj (unsafeIndexM a j i) * unsafeIndexM u j k | j <- [0..n - 1]]
        
        pythag xs = foldl1' pythag2 (sortBy (flip cmp) xs)
        pythag2 x 0 = x
        pythag2 x y = absa * sqrt (1 + (absb/absa)^2)
            where 
                absa = abs x
                absb = abs y
        
--         pythag xs = pythag2 (sortBy (flip (comparing abs)) xs)
--         pythag2 (x:ys) = absx * sqrt (foldl' (+) 1 s)
--             where
--                 absx = abs x
--                 s = [ (abs y / absx) ^ 2
--                     | y <- ys
--                     ]

