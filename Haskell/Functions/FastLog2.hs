module Functions.FastLog2 where

import Foreign
import Math.Polynomial.Chebyshev

packFloat :: Int32 -> Float
packFloat f = unsafePerformIO $ alloca $ \b -> do
    poke b f
    peek (castPtr b)

unpackFloat :: Float -> Int32
unpackFloat i = unsafePerformIO $ alloca $ \b -> do
    poke b i
    peek (castPtr b)

x_frac f = packFloat ((x .&. 0x807fffff) + 0x3f800000)
    where x = unpackFloat f

-- |extremely crude estimate of @logBase 2@, exact for powers of two
fastLog2 :: Float -> Float
fastLog2 f = fromIntegral log_2 + log_2_frac
    where
        -- unpack to an integer so we can twiddle the bits
        x = unpackFloat f
        
        -- overwrite the exponent to scale 'f' to the range [1,2).
        x_frac = packFloat ((x .&. 0x807fffff) + 0x3f800000)
        
        -- get the integer part of the logarithm as (exponent - 1), and 
        -- subtract an additional 1 because log_2_frac is an approximation
        -- of 1 + log2 x_frac.
        log_2 = ((x `shiftR` 23) .&. 0xff) - 0x81
        
        -- approximate 1 + logarithm of x_frac
        log_2_frac = approx x_frac

-- The actual approximation at the heart of fastLog2.  This accepts an argument
-- @x@ in the range [1,2) and should return an approximation of @2 + logBase 2 x@.
approx :: Float -> Float
-- original version (error typically around 1e-3):
-- approx x = ((-1/3) * x + 2) * x + 1/3

-- 4-term chebyshev series linearly transformed to place outermost nodes at
-- 1 and 2 (so that fastLog2 will be exact at power-of-2 arguments):
-- approx x = evalChebyshevSeries ser (1.847759 * (x - 1.5))
--     where ser = [2.5355391,0.53865826,-5.025989e-2,6.1268853e-3]

-- same chebyshev series expanded to a simple polynomial. Theoretically, very
-- slightly less stable than the 'evalChebyshevSeries' version because of the
-- several terms > 1.  Error typically around 1e-4.
approx x = ((0.15460588 * x - 1.0389228) * x + 3.0345273) * x - 0.1502104

-- yet another version, based on flattening the function and refitting a 
-- chebyshev series to it.
-- approx = evalChebyshevSeries [-0.669689,3.1505058,-0.519469,3.8652338e-2]

err :: Float -> Double
err x = realToFrac (fastLog2 x) - logBase 2 (realToFrac x)