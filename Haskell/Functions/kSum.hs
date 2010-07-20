module KSum where

ksum :: Num a => [a] -> a
ksum [] = 0
ksum (x:xs) = go 0 x xs
    where
        go c sum xs | c `seq` sum `seq` False = undefined
        go c sum [] = sum
        go c sum (x:xs) = go c' t xs
            where
                y = x - c
                t = sum + y
                c' = (t-sum) - y