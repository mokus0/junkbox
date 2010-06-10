module BriansBrain where

import Data.List

board w h dist = sequence
    [ sequence
        [ dist w h
        | j <- [1..h]
        ]
    | i <- [1..w]
    ]

torusWindow xs = segments 3 (last xs : xs ++ [head xs])
    where
        segments n ys =
            [ take n y
            | y <- tails ys
            , not . null $ drop (n-1) y
            ]