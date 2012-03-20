import Data.VectorSpace
import Graphics.Gloss
import Math.Spline
import Math.Spline.BSpline

spl = cSpline 
    [ (-10,     1, -1)
    , (-8,      1,  1)
    , (-6,      3, -1)
    , (-5.75, -pi,  1)
    , (-5.5,    1, -1)
    , (-5.25,   1,  1)
    , (-2,      0,  2) 
    , (0,       1,  1)
    , (0.5,   1.5,  1)
    , (1,       1,  1)
    , (2,       2,  1)
    , (3,      -3,  1)
    , (4,    3.75,  0)
    , (5,       3, -1)
    , (8,       0, -1)
    , (8.1,     0,  1)
    , (10,      0, -1)
    ]

spl' = differentiateBSpline (toBSpline spl)

picture
    = scale 24.5 24.5
    $ pictures
        [ box (-10, -10) (10, 10)
        , color (greyN 0.75) $            axis 20 (-10) 10
        , color (greyN 0.75) $ rotate 90 (axis 20 (-10) 10)
        , color blue  (plot 250 (-10) 10 (evalSpline spl))
        , color green (plot 250 (-10) 10 (evalSpline spl'))
        ]


box (x0, y0) (x1,y1) = line
    [ (x0, y0), (x0, y1), (x1, y1), (x1, y0)
    , (x0, y0)
    ]

axis n x0 x1 = pictures
    $   line [(x0,   0), (x1,  0)]
    : [ line [(x, -0.1), (x, 0.1)] | x <- ticks n x0 x1 ]

ticks n x0 x1 = map tick [0..n]
    where tick i = lerp x0 x1 (fromInteger i / fromInteger n)

plot n x0 x1 f = line [ (x, f x) | x <- ticks n x0 x1]

