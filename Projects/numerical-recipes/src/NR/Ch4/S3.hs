module NR.Ch4.S3 where

import NR.Ch3.S1
import NR.Ch3.S2
import NR.Ch4.S2

import qualified Data.Vector as V

-- |@romb qrule dh jmax k f a b@ uses a quadrature rule @qrule@ with ratio @dh@ 
-- between successive squared stepsizes to build a table mapping step sizes to
-- estimated integrals, then performs polynomial interpolation of order @k@ on
-- the resulting table to provide an improved estimate of the integral, using
-- a maximum of @jmax@ estimates from @qrule@.
--
-- The integral being computed is over @f@ from @a@ to @b@.  The elements of
-- the returned list are pairs, the first element of which is an estimate of
-- the integral and the second of which is an estimate of the interpolation
-- error.
romb qrule dh jmax k f a b = 
    [ polyRawInterp (V.slice j k xys) 0 0
    | j <- [0 .. jmax-k]
    ]
    where
        ys  = qrule f a b
        xs  = iterate (* dh) 1
        xys = V.generate jmax $ \j -> zip xs ys !! j
        -- (need a V.fromListN or something)

-- |@qromb f a eps@ estimates the integral of @f@ from @a@ to @b@ using
-- Romberg interpolated integration based on the trapezoid rule.
qromb f a b eps = converge (romb trapzd 0.25 jmax k f a b)
    where
        jmax = 20
        k = 5
        
        converge ((x, dy):xs) 
            | abs dy <= eps * abs x     = (x, dy)
            | otherwise                 = converge xs
        converge _ = error "qromb: too many steps"
