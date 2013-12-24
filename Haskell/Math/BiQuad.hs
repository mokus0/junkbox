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
    where d = negate a1 - sqrt (a1*a1 - 4*a2)

directForm1 BiQuad{..} !x0 = do
    (x1, x2, y1, y2) <- get
    let !y0 =              a1 * y1 + a2 * y2
             + b0 * x0   + b1 * x1 + b2 * x2
    put (x0, x1, y0, y1)
    
    return y0

directForm2 BiQuad{..} !x = do
    (w1, w2) <- get
    let !w =      x - a1 * w1 - a2 * w2
    put (w, w1)
    
    return $! (b0 * w + b1 * w1 + b2 * w2)

run filt = flip (evalState . mapM filt)