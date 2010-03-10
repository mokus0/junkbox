{-# LANGUAGE RecordWildCards, BangPatterns #-}
module GSL.Cheb where

class Eps a where
    eps :: a

instance Eps Double where
    eps = 2.2204460492503131e-16

-- A computed result with an error estimate
data Result a e = Result
    { val :: a
    , err :: e
    } deriving (Eq, Ord, Show)

data ChebSeries a = ChebSeries
    { chebCoeffs    :: [a]
    , chebOrder     :: Int
    , chebA         :: a
    , chebB         :: a
    } deriving (Eq, Show)

cheb_eval_e ChebSeries{..} x = step chebOrder 0 0 0
    where
        y   = (2 * x - chebA - chebB) / (chebB - chebA)
        y2  = 2 * y
        
        step !j !d !dd !e 
            | j > 0     = step (j-1) (y2*d - dd +       (chebCoeffs !! j)) d (e + abs (y2 * d) + abs dd +       abs (chebCoeffs !! j))
            | otherwise = finish     (y *d - dd + 0.5 * (chebCoeffs !! 0))   (e + abs (y  * d) + abs dd + 0.5 * abs (chebCoeffs !! 0))
        
        finish !d !e = Result d (eps * e + abs (chebCoeffs !! chebOrder))
