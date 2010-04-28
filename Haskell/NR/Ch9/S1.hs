module NR.Ch9.S1 where

-- |Given a function and an initial guessed range x1 to x2, this function
-- expands the range geometrically until a root is bracketed by the returned
-- values, returning a list of the successively expanded ranges.  The
-- list will be finite if and only if the sequence yields a bracketing pair.
zbrac f x1 x2
    | x1 == x2  = error "zbrac: empty range"
    | otherwise = go x1 (f x1) x2 (f x2)
    where
        ntry = 50
        factor = 1.6
        go x1 f1 x2 f2
            | signum f1 /= signum f2    = [(x1, x2)]
            | abs f1 < abs f2           = (x1, x2) : go x1' (f x1') x2 f2
            | otherwise                 = (x1, x2) : go x1 f1 x2' (f x2')
            where 
                x1' = x1 - factor * w
                x2' = x2 + factor * w
                w = x2 - x1

-- |Given a function defined on the interval [x1,x2], subdivide the interval
-- into n equally spaced segments and search for zero crossings of the function.
-- The returned list will contain all bracketing pairs found.
zbrak f x1 x2 n = 
    [ (x1, x2)
    | ((x1, y1), (x2, y2)) <- zip xys (tail xys)
    , signum y1 /= signum y2
    ]
    where
        dx = (x2 - x1) / fromIntegral n
        xs = x1 : [x1 + dx * fromIntegral i | i <- [1..n]]
        xys = map (\x -> (x, f x)) xs

-- |Using bisection, return a root of a function known to lie between x1 and x2.
-- The root will be refined till its accuracy is +-xacc.
rtbis f x1 x2 xacc = convergeToEps "rtbis" xacc (bisect f x1 x2)

converge eps = convergeToEps "converge" eps
convergeToEps loc eps = fst . convergeTo loc 100 (\(x,dx) -> abs dx <= eps)

-- |Given a (non-empty) list of improving estimates for a value, find either
-- the final  estimate or the first one satisfying a predicate.  Limit
-- the search to i given number 'n' of tries and fail if that number is 
-- exceeded, using the given string as a description of the location of failure
-- (typically a function name).
convergeTo loc _ _ [] = error (loc ++ ": empty list")
convergeTo loc n p xs = go n xs
    where
        go n [x] = x
        go n (x:xs)
            | n <= 0    = error (loc ++ ": too many steps")
            | p x       = x
            | otherwise = go (n+1) xs

-- |Bisect an interval in search of the root, returning at each step a guess at
-- the location of the root and the width of the current interval.  In the event
-- of discovering an exact zero, returns the corresponding abscissa as the last
-- element of the list.
--
-- Each element of the returned list is a pair (x,dx) where f x <= 0 and f (x+dx) >= 0.
bisect f x1 x2
    | f1 < 0    = go (x2-x1) x1 (f x1)
    | otherwise = go (x1-x2) x2 (f x2)
    where
        f1 = f x1
        f2 = f x2
        
        go dx x fx = case fMid `compare` 0 of
            LT ->  (x, dx) : go dx2 xMid fMid
            EQ ->  [(xMid, 0)]
            GT ->  (x, dx) : go dx2 x fx
            where
                dx2 = dx * 0.5
                xMid = x + dx2
                fMid = f xMid
