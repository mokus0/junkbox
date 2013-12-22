module Math.PolarComplex where

import Math.Angle

data Complex = Complex !Double !Angle
i = Complex 1 (turns 0.25)
conj (Complex r t) = Complex r (-t)

instance Show Complex where
    showsPrec p (Complex r t) 
        | y == 0    = showsPrec p x
        | otherwise = showParen (p > 6)
            ( showsPrec 6 x
            . showString " + "
            . showsPrec 7 y
            . showString " * i"
            )
        where
            x = r * cos (toRadians t)
            y = r * sin (toRadians t)

instance Num Complex where
    Complex  r0 0 + Complex r1 t1 = Complex r2 t2
        where
            x1 = r1 * cos (toRadians t1)
            
            x2 = r0 + x1
            y2 =      r1 * sin (toRadians t1)
            
            r2 = sqrt (r0^2 + r1^2 + 2 * r0 * x1)
            t2 = radians (atan2 y2 x2)
    Complex r0 t0 + Complex r1 t1 = Complex r2 (t2 + t0)
        where ~(Complex r2 t2) = Complex r0 0 + Complex r1 (t1 - t0)
    
    Complex r0 t0 * Complex r1 t1 = Complex (r0 * r1) (t0 + t1)
    
    negate   (Complex r t) = Complex r (t + turns 0.5)
    abs      (Complex r _) = Complex r 0
    signum c@(Complex 0 _) = c
    signum   (Complex _ t) = Complex 1 t
    
    fromInteger i
        | i >= 0    = Complex (fromInteger   i)  0
        | otherwise = Complex (fromInteger (-i)) (turns 0.5) 

instance Eq Complex where
    Complex r0 t0 == Complex r1 t1
        = r0 == r1 && (r0 == 0 || t0 == t1)
