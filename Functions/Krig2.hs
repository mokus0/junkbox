{-# LANGUAGE
        RankNTypes, RecordWildCards, FlexibleContexts
  #-}
module Krig2 where

import Control.Monad.ST
import Data.Matrix.Types
import Data.Matrix.Mutable
import Data.Matrix.Alias
import Data.Matrix.Math
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

type Variogram t = forall vec1 vec2. (Vector vec1 t, Vector vec2 t) => vec1 t -> vec2 t -> t

-- |use this in place of Nothing in the 'err' parameter to 'krig' 
-- to help out the type inference system (nails down vec2, which 'Nothing' 
-- leaves dangling)
noErr :: Maybe (IVector a)
noErr = Nothing

distVGram :: Floating t => (t -> t) -> Variogram t
distVGram vgram x y = vgram (dist ndim x y)
    where ndim = min (vecElems x) (vecElems y)

krig :: (Matrix mat a, Vector vec a, Vector vec2 a, Floating a, Ord a)
     => mat a -> vec a -> Variogram a -> Maybe (vec2 a) -> Krig a
krig x yy vgram err = Krig
        { krigNPt   = npt
        , krigNDim  = ndim
        , krigVi    = vi
        , krigYVi   = yvi
        , krigVStar = \xstar i -> if i < npt
                                    then vgram (row i x) xstar
                                    else 1
        }

    where
        npt   = matRows x
        ndim  = matCols x
        
        errsq i = fmap (\e -> indexV e i ^ 2) err
        subErrSq i = maybe id subtract (errsq i)
        
        y = fvector (npt + 1) (\i -> if i < npt then indexV yy i else 0)
        
        v = imatrix (npt + 1) (npt + 1) $ \i j ->
                let vgram_ij = vgram (row i x) (row j x)  -- or?: vgram (row j x) (row i x)
                in case (i < npt, j < npt) of
                    (True, True)
                        | i > j     -> indexM v j i
                        | i == j    -> subErrSq i vgram_ij
                        | otherwise -> vgram_ij
                    (False, False) -> 0
                    other   -> 1
        vi = ludcmp v
        yvi = luSolveV vi y

interp Krig{..} xstar = (estval, esterr)
    where
        vstar = ivector (krigNPt + 1) (krigVStar xstar)
        estval = krigYVi `dot` vstar
        
        dstar = luSolveV krigVi vstar
        esterr = sqrt (max 0 (dstar `dot` vstar))

interp_ Krig{..} xstar = estval
    where
        vstar = fvector (krigNPt + 1) (krigVStar xstar)
        estval = krigYVi `dot` vstar

{-# INLINE powVarGram #-}
powVarGram :: (Matrix mat a, Vector vec a, Floating a) 
           => mat a -> vec a -> a -> Maybe a -> Variogram a
powVarGram x y beta nug = distVGram $ \r -> addNugSq (alpha * (r ** beta))
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
                
krig' :: (Matrix mat Double, Vector vec Double) =>
     mat Double -> vec Double -> Krig Double
krig' x y = krig x y vgram noErr
    where
        vgram :: Variogram Double
        vgram = powVarGram x y 1.5 Nothing

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
