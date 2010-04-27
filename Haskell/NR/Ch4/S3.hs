module NR.Ch4.S3 where

import NR.Ch3.S1
import NR.Ch3.S2
import NR.Ch4.S2

import qualified Data.Vector as V

romb qrule jmax k f a b = 
    [ polyRawInterp (V.slice j k xys) 0 0
    | j <- [0 .. jmax-k]
    ]
    where
        ys  = qrule f a b
        xs  = iterate (* 0.25) 1
        xys = V.generate jmax $ \j -> zip xs ys !! j
        -- (need a V.fromListN or something)

qromb f a b eps = converge (romb trapzd jmax k f a b)
    where
        jmax = 20
        k = 5
        
        converge ((x, dy):xs) 
            | abs dy <= eps * abs x     = (x, dy)
            | otherwise                 = converge xs
        converge _ = error "qromb: too many steps"
