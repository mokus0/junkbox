-- Digital biquadratic filters
-- http://en.wikipedia.org/wiki/Digital_biquad_filter
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Math.BiQuad where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Bits
import Data.Complex
import Data.IORef
import Data.Int
import Data.Random
import Data.Word
import Math.Polynomial

data BiQuad a = BiQuad
    {     a1, a2    :: !a
    , b0, b1, b2    :: !a
    } deriving (Eq, Ord, Read, Show, Functor)

transfer BiQuad{..} z
    = evalPoly (poly LE [b0 , b1 , b2]) zInv
    / evalPoly (poly LE [1  , a1 , a2]) zInv
    where zInv = recip z

poles BiQuad{..} = (2 * a2 / d, 0.5 * d)
    where d = negate a1 - sqrt (a1*a1 - 4*a2)

directForm1 BiQuad{..} !x0 = do
    (x1, x2, y1, y2) <- get
    let !y0 =  b0 * x0 + b1 * x1 + b2 * x2
             - a1 * y1 - a2 * y2
    put (x0, x1, y0, y1)
    
    return y0

directForm2 BiQuad{..} !x = do
    (w1, w2) <- get
    let !w = x - a1 * w1 - a2 * w2
    put (w, w1)
    
    return $! (b0 * w + b1 * w1 + b2 * w2)

directForm2t BiQuad{..} x = do
    (d1, d2) <- get
    let y   = b0 * x          + d1
        d1' = b1 * x - a1 * y + d2
        d2' = b2 * x - a2 * y
    put (d1', d2')
    return (y, d1', d2')

run filt = flip (evalState . mapM filt)

candleFilter :: BiQuad Double
candleFilter = BiQuad (-0.87727063) 0.31106039 0.10844744 0.21689488 0.10844744

swap ~(a,b) = (b,a)

runIO :: Num a => RVar a -> BiQuad a -> IO (RVarT IO a)
runIO dist filt = do
    st <- newIORef (0,0)
    return $ do
        x <- sample dist
        lift (atomicModifyIORef' st (swap . runState (directForm2 filt x)))

-- fixed point version of 2-stage IIR candle filter.
-- (2nd-order butterworth low-pass filter with sample rate of 60Hz,
-- cutoff at 8 Hz, as described by Park Hays at [0])
-- 
-- sigbits/exponent accounting (in {- s/e -} comments) should be thorough
-- enough that, assuming no silly errors, it can pretty easily be extended
-- to a proof that overflow is impossible.
-- 
-- Rounding can be improved slightly in a couple places by adding terms
-- before rounding, but I didn't do that here in order to keep the structure
-- clearer.  It may also be possible to get a bit more overall precision by
-- changing parameters of the filter to get coefficients with fewer
-- significand bits (perhaps by slightly altering the sample rate, for
-- example).
-- 
-- It may also be worth looking at direct-form 1; I suspect an 8-bit direct
-- form implementation may have enough precision to be pleasant to watch.
-- It'd use the same amount of state memory as this one, but a fair bit
-- less compute time.
-- 
-- [0] http://inkofpark.wordpress.com/2013/12/23/arduino-flickering-candle/
candle16_df2t :: Monad m => Int8 -> StateT (Int16, Int16) m (Int16, Int16, Int16)
candle16_df2t !x {- 7/5 -} = do
    let a1 {- 3/3  -} = round ((-0.87727063) * 2 ^ 3 )
        a2 {- 4/5  -} = round (  0.31106039  * 2 ^ 5 )
        b0 {- 7/9  -} = round (  0.10844744  * 2 ^ 10)
        b1 {- 7/10 -} = round (  0.21689488  * 2 ^ 9 )
        b2 {- 7/10 -} = round (  0.10844744  * 2 ^ 10)
    
    (d1, d2) <- get
    let !y   {- 15/13 -} = (((b0 * fromIntegral x) `shiftR` 2)                        ) + (d1 `shiftR` 0)
        !d1' {- 15/13 -} = (((b1 * fromIntegral x) `shiftR` 1) - (a1 * (y `shiftR` 3))) + (d2 `shiftR` 1)
        !d2' {- 15/14 -} = (((b2 * fromIntegral x) `shiftR` 1) - (a2 * (y `shiftR` 4)))
            -- note: d2's sigbits depends on value of a2
    put (d1', d2')
    return (y, d1', d2')

main = do
    st <- newIORef (0, 0)
    
    replicateM (2^16) $ do
        x <- sample (normal 0 32) :: IO Double
        atomicModifyIORef' st (swap . runState (candle16_df2t (round x)))

foo = do
    st <- newIORef (0, 0)
    
    replicateM (2^16) $ do
        x <- sample stdNormal :: IO Double
        atomicModifyIORef' st (swap . runState (directForm2t candleFilter x))