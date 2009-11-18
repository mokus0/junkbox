module Math.Converge where

converge :: Eq a => [a] -> a
converge = head . convergeBy (==)

convergeBy (==) [] = error "convergeBy: empty list"
convergeBy (==) (x:xs) = go x xs
    where
        go x [] = [x]
        go x zs@(y:ys)
            | x == y    = zs
            | otherwise = go y ys

overconverge n = head . overconvergeBy (==) n
overconvergeBy (==) n (x:xs) = go 0 x xs 
    where
        go i x [] = [x]
        go i x zs@(y:ys)
            | x == y    = if i >= n
                then zs
                else go (i+1) y ys
            | otherwise = go 0 y ys

convergeIntervalsBy (==) out = go
    where
        go ((a,b):rest)
            | a == b    = out a b
            | otherwise = go rest

convergeIntervals xs = convergeIntervalsBy (==) const

