-- Digital biquadratic filters
-- http://en.wikipedia.org/wiki/Digital_biquad_filter
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Math.BiQuad where

import Control.Monad.Trans.State
import Data.Complex
import Math.Polynomial

data BiQuad a = BiQuad
    {     a1, a2    :: !a
    , b0, b1, b2    :: !a
    } deriving (Eq, Ord, Read, Show)

transfer BiQuad{..} z
    = evalPoly (poly LE [b0 , b1 , b2]) zInv
    / evalPoly (poly LE [1  , a1 , a2]) zInv
    where zInv = recip z

poles BiQuad{..} = (2 * a2 / d, 0.5 * d)
    where
        d = negate a1 - sqrt (a1*a1 - 4*a2)
            --else negate a1 + sqrt (a1*a1 - 4*a2)

directForm1 BiQuad{..} !x_n = do
    (x_nm1, x_nm2, y_nm1, y_nm2) <- get
    let !y_n =              a1 * y_nm1 + a2 * y_nm2
             + b0 * x_n   + b1 * x_nm1 + b2 * x_nm2
    put (x_n, x_nm1, y_n, y_nm1)
    
    return y_n

directForm2 BiQuad{..} !x_n = do
    (w_nm1, w_nm2) <- get
    let !w_n =      x_n - a1 * w_nm1 - a2 * w_nm2
        !y_n = b0 * w_n + b1 * w_nm1 + b2 * w_nm2
    put (w_n, w_nm1)
    
    return y_n

run filt = flip (evalState . mapM filt)