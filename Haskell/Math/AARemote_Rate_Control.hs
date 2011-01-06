{-# LANGUAGE RecordWildCards #-}
module Math.AARemote_Rate_Control where

import Numeric.FFT
import Data.Complex

data Params t = Params
    { minFramesPerTick      :: t
    , targetFramesPerTick   :: t
    , maxFramesPerTick      :: t
    , framesPerTickAvgDecay :: t
    } deriving (Eq, Show)

data SystemState t = SystemState
    { framesPerTickAvg      :: t
    } deriving (Eq, Show)

getFrameAllowance Params{..} SystemState{..} 
    | framesPerTickAvg <= minFramesPerTick  = maxFramesPerTick
    | framesPerTickAvg >= maxFramesPerTick  = minFramesPerTick
    | otherwise = targetFramesPerTick + y
    where
        a = minFramesPerTick - targetFramesPerTick;
        x = framesPerTickAvg - targetFramesPerTick;
        b = maxFramesPerTick - targetFramesPerTick;
    
        k = a * b / (a + b);
        y = k * x / (x - k);
        

logFrames numFrames Params{..} st@SystemState{..} = st 
    { framesPerTickAvg = a * framesPerTickAvg + b * numFrames }
    where
        b = framesPerTickAvgDecay;
        a = 1 - b;


-- Consider 'framesPerTickAvg' as a filter over 'numFrames'.  What does it look like?
-- I don't really know whether this is the right way to go about computing a transfer
-- function, but here's my stab at it.  In particular, i'm not sure whether it's valid
-- to use the Fourier transform instead of the Laplace transform - or rather, how the
-- "i" in the conversion propagates through the defition of the transfer function.
-- 
-- Looking at the output, expFilter looks like a constant-k low-pass filter, 
-- or at least somewhat like one.  Intuitively this would make sense too - the
-- real part of the impedance corresponds to the damping by (1-a) and the 
-- imaginary part corresponds to the accumulator.
-- 
-- In any case, it's definitely a low-pass filter, and the 'a' parameter
-- (1 - framesPerTickAvgDecay) is basically the "aggressiveness" of the filter.
-- In other words, the lower the value of framesPerTickAvgDecay, the more
-- aggressively high frequencies (wild oscillations) are rejected, but the more
-- slowly the system responds (the higher the imaginary part of the response)

expFilter a []      = []
expFilter a (x:xs)  = loop x xs
    where
        loop y []       = [y]
        loop y (x:xs)   = y : loop ((1-a) * x + a * y) xs

realFilt f = map (:+ 0) . f . map realPart

transferFunction lgSz filt = 
    [ fft (filt input) !! (freq-1)
    | freq <- [1..sz]
    , let input = ifft (pure freq sz)
    ]
    where
        sz = 2^lgSz
pure n l = replicate (n-1) 0 ++ 1 : replicate (l-n) 0


convolve       xs ys = idft (zipWith (*) (dft xs) (dft ys))
deconvolve     xs ys = idft (zipWith (/) (dft xs) (dft ys))
fastConvolve   xs ys = ifft (zipWith (*) (fft xs) (fft ys))
fastDeconvolve xs ys = ifft (zipWith (/) (fft xs) (fft ys))

invert     ys = idft (map recip (dft ys))
fastInvert ys = ifft (map recip (fft ys))

cancellation     filt sig = deconvolve     (filt sig) sig
fastCancellation filt sig = fastDeconvolve (filt sig) sig
