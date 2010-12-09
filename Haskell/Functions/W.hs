-- the Lambert W function (on real line)
module Functions.W where

import Math.Sequence.Converge

e :: Floating a => a
e = exp 1

xMin :: Floating a => a
xMin = (-1) / e

eps :: RealFloat a => a
eps = eps'
    where
        eps' = encodeFloat 1 (1 - floatDigits eps')

w0 x = case x `compare` xMin of
    LT -> error "w0: x < -1/e" 
    EQ -> -1
    GT -> convergeTo 0 eps (iterate next initial)
    where
        initial = log (x+1)
        -- this can go into cycles; need cycle-breaking logic or something
        -- (for example, w0 (-0.36787944066384143))
        next w = let a = w * exp w
                  in w - (a - x) / (exp w + a)