import Graphics.Gloss

-- mod-n multiplication tables

n = 23

picture
    = scale (500 / n) (500 / n)
    $ translate (-0.5 * n) (-0.5 * n)
    $ grid n n $ \i j -> number (((i+1) * (j+1)) `mod` n)

grid n m cell = pictures $ concat
    [ [ line [(flt i,     0), (flt i, flt m)] | i <- [0..n]]
    , [ line [(    0, flt i), (flt n, flt i)] | i <- [0..m]]
    , [ translate (flt i + 0.5) (flt j + 0.5)
        $ scale 0.5 0.5 $ cell i (m-j-1)
      | i <- [0..n-1]
      , j <- [0..m-1]
      ]
    ]

flt = fromIntegral

number x
    = translate (0.4 * flt (w-w') - 0.82 + 0.17 / flt w)
                (0.0              - 0.20 - 0.65 / flt w)
    $ scale sz sz
    $ text $ show x
    where
        w = length (show n)
        w' = length (show x)
        sz = 0.005 + 0.02 / flt w
    