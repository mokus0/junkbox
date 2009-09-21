{-
 -      ``Circles''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}

module Math.Circles where

import Data.Complex

circumscribe a b c = case circumscribeComplex (mkC a) (mkC b) (mkC c)
    of (x :+ y, r) -> ((x,y), r)
    where mkC = uncurry (:+)

circumscribeComplex 0 1 c@(re :+ im) = (center, magnitude center)
    where
        center = 0.5 :+ (realPart (c * conjugate c) - re) / (2 * im)
circumscribeComplex 0 b c = case circumscribeComplex 0 1 (c / b)
    of (c, r) -> (c * b, r * magnitude b)
circumscribeComplex a b c = case circumscribeComplex 0 (b-a) (c-a)
    of (c, r) -> (a + c, r)

-- first cut; saved for posterity (or something)
circumscribeFoo a b c = cs3 a b c
    where
        cs3 (0,0) b c   = cs2 b c
        cs3 (ax,ay) (bx,by) (cx,cy) = case cs2 (bx - ax, by - ay) (cx - ax, cy - ay) of
            ((x,y), r) -> ((x + ax, y + ay), r)
        
        cs2 (1,0) c = cs1 c
        cs2 (s,0) c = case cs1 (unscale s c) of
            (c, r) -> (scale s c, r * s)
        cs2 b c  = case cs1 (unscale s . unrot t $ c) of
            (c, r) -> (scale s . rot t $ c, r * s)
            where
                s = mag b
                t = arg b
        
        cs1 (x,y) = (center, radius)
            where
                center = (0.5, (x^2 + y^2 - x) / (2 * y))
                radius = mag center
        
        mag (x,y) = sqrt (x^2 + y^2)
        arg (x,y) = atan2 y x
        
        rot dt v = case (mag v, arg v) of
            (r,t) -> unPolar r (t + dt)
            where
                unPolar r t = (r * cos t, r * sin t)
        unrot = rot . negate    
        
        scale s (x,y) = (x*s, y*s)
        unscale = scale . recip

z xs = (sum [ x^2 | x <- xs] - 1) / (sum xs - 1)
r n z = sqrt (n * z^2  -  2 * z  +  1)
zr xs = (z xs, r (fromIntegral $ length xs) (z xs))