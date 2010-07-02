{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Math.Root.Bracket where

brackets :: (Eq a, Num b) => (a -> b) -> (a,a) -> Bool
brackets f (x1,x2)
    | x1 == x2  = f x1 == 0
    | otherwise = signum f1 /= signum f2
    where
        f1 = f x1; f2 = f x2

-- |@bracket f x1 x2@: Given a function and an initial guessed range x1 to x2,
-- this function expands the range geometrically until a root is bracketed by 
-- the returned values, returning a list of the successively expanded ranges.  
-- The list will be finite if and only if the sequence yields a bracketing pair.
bracket :: (Fractional a, Num b, Ord b) =>
           (a -> b) -> a -> a -> [(a, a)]
bracket f x1 x2
    | x1 == x2  = error "bracket: empty range"
    | otherwise = go x1 (f x1) x2 (f x2)
    where
        factor = 1.618
        go x1 f1 x2 f2
            | signum f1 /= signum f2    = [(x1, x2)]
            | abs f1 < abs f2           = (x1, x2) : go x1' (f x1') x2 f2
            | otherwise                 = (x1, x2) : go x1 f1 x2' (f x2')
            where 
                x1' = x1 - factor * w
                x2' = x2 + factor * w
                w = x2 - x1

-- |@subdivideAndBracket f x1 x2 n@: Given a function defined on the interval
-- [x1,x2], subdivide the interval into n equally spaced segments and search 
-- for zero crossings of the function.  The returned list will contain all 
-- bracketing pairs found.
subdivideAndBracket :: (Num b, Fractional a, Integral c) =>
                       (a -> b) -> a -> a -> c -> [(a, a)]
subdivideAndBracket f x1 x2 n = 
    [ (x1, x2)
    | ((x1, y1), (x2, y2)) <- zip xys (tail xys)
    , signum y1 /= signum y2
    ]
    where
        dx = (x2 - x1) / fromIntegral n
        xs = x1 : [x1 + dx * fromIntegral i | i <- [1..n]]
        xys = map (\x -> (x, f x)) xs

