{-# LANGUAGE
        RankNTypes, RecordWildCards
  #-}
module Krig where

import Control.Monad.ST
import Data.Matrix.Types
import Data.Matrix.Mutable
import Data.Matrix.Alias
import Data.Matrix.Algorithms.LUDecomp

import Control.Monad.ST
import Data.StateRef
import Data.List

data Krig t = Krig
    { krigNPt   :: !Int
    , krigNDim  :: !Int
    , krigVi    :: LUDecomp IMatrix t
    , krigYVi   :: IVector t
    , krigVStar :: Vector vec t => vec t -> Int -> t
    }

type Variogram t = t -> t

krig :: (Matrix mat a, Vector vec a, Floating a, Ord a)
     => mat a -> vec a -> Variogram a -> Maybe a -> Krig a
krig x yy vgram err = Krig
        { krigNPt   = npt
        , krigNDim  = ndim
        , krigVi    = vi
        , krigYVi   = yvi
        , krigVStar = \xstar i -> if i < npt
                                    then vgram (dist ndim xstar (row i x))
                                    else 1
        }

    where
        npt   = matRows x
        ndim  = matCols x
        y = fvector (npt + 1) (\i -> if i < npt then indexV yy i else 0)
        
        v = imatrix (npt + 1) (npt + 1) $ \i j -> case (i < npt, j < npt) of
                (True, True)
                    | i > j     -> indexM v j i
                    | i == j    -> vgram 0 - maybe 0 (^2) err
                    | otherwise -> vgram (dist ndim (row i x) (row j x))
                (False, False) -> 0
                other   -> 1
        vi = ludcmp v
        yvi = luSolveV vi y

same f a b 
    | a == b    = a
    | otherwise = error (f ++ ": equality assertion failed (" ++ show a ++ " /= " ++ show b ++ ")")

dot v1 v2 = sum' [indexV v1 i * indexV v2 i | i <- [0..n-1]]
    where n = same "dot" (vecElems v1) (vecElems v2)

row :: (Matrix mat a) => Int -> mat a -> IAlias Vec a
row n mat = Row n (IMat mat)

interp Krig{..} xstar = (estval, esterr)
    where
        vstar = fvector (krigNPt + 1) (krigVStar xstar)
        estval = krigYVi `dot` vstar
        
        dstar = luSolveV krigVi vstar
        esterr = sqrt (max 0 (dstar `dot` vstar))

interp_ krig xstar = fst (interp krig xstar)

{-# INLINE powVarGram #-}
powVarGram :: (Matrix mat a, Vector vec a, Floating a) 
           => mat a -> vec a -> a -> Maybe a -> Variogram a
powVarGram x y beta nug = \r -> addNugSq (alpha * (r ** beta))
    where
        nugsq = fmap (^2) nug
        addNugSq = maybe id (+)      nugsq
        subNugSq = maybe id subtract nugsq
        
        npts = matRows x
        ndim = matCols x
        
        alpha = num/denom
            where
                (num, denom) = sum2
                    [ ( rb * subNugSq (0.5 * dy ^ 2) -- numerator component
                      , rb^2 -- denominator component
                      )
                    | i <- [0 .. npts-1]
                    , j <- [0 .. npts-1]
                    , let rb = distsq ndim (row i x) (row j x) ** (0.5 * beta)
                          dy = indexV y i - indexV y j
                    ]
                

krig' x y = krig x y vgram Nothing
    where
        vgram = powVarGram x y 1.5 Nothing

distsq_ x y = distsq n x y
    where n = same "distsq_" (vecElems x) (vecElems y)


dist n x1 x2 = sqrt (distsq n x1 x2)
distsq n x y = sum'
    [ (indexV x k - indexV y k) ^ 2
    | k <- [0 .. n-1]
    ]

-- strict sum
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

-- strict sum of pairs
sum2 :: (Num a, Num b) => [(a,b)] -> (a,b)
sum2 = foldl' add2 (0,0)
    where
        add2 (x1,y1) (x2,y2) = x `seq` y `seq` (x,y)
            where x = x1+x2; y = y1+y2
