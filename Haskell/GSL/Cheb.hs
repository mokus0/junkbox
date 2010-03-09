{-# LANGUAGE RecordWildCards #-}
module GSL.Cheb where

import Control.Monad.ST
import Data.STRef

class Eps a where
    eps :: a

instance Eps Double where
    eps = 2.2204460492503131e-16

data Result a e = Result
    { val :: a
    , err :: e
    } deriving (Eq, Ord, Show)


data ChebSeries a = ChebSeries
    { chebCoeffs    :: [a]
    , chebOrder     :: Int
    , chebA         :: a
    , chebB         :: a
    }

cheb_eval_e ChebSeries{..} x = runST $ do
    d  <- newSTRef (0,0,0)
    
    let y   = (2 * x - chebA - chebB) / (chebB - chebA)
        y2  = 2 * y
    
    sequence_
        [ modifySTRef d $ \(d,dd,e) -> 
            ( y2*d - dd + (chebCoeffs !! j)
            , d
            , e + abs (y2 * d) + abs dd + abs (chebCoeffs !! j)
            )
            
        | j <- [chebOrder, chebOrder - 1 .. 1]
        ]
    
    modifySTRef d $ \(d,dd,e) -> 
        ( y*d - dd + 0.5 * (chebCoeffs !! 0)
        , dd
        , e + abs (y * d) + abs dd + 0.5 * abs (chebCoeffs !! 0)
        )
    
    (d,dd,e) <- readSTRef d
    return (Result d (eps * e + abs (chebCoeffs !! chebOrder)))
