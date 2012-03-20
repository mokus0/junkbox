module Math.PeasantMultiplication where

import Data.Maybe
import Text.PrettyPrint
import Text.Printf

mult :: Integer -> Integer -> ([(Integer, Integer, Maybe Integer)], Integer)
mult x y
    | x > 0 && y > 0    = go x y [] [] []
    | otherwise         = error "mult: positive numbers required"
    where
        go 0 y xs ys os = (reverse (zip3 xs ys os), sum (catMaybes os))
        go x y xs ys os = go (x `div` 2) (y * 2) (x:xs) (y:ys) (o:os)
            where o = if odd x then Just y else Nothing

pprMult x y =
    vcat [ text $ printf "%d\t%d\t%s\t" x y (maybe "" show mbOdd)
         | (x,y,mbOdd) <- table]
 <+> integer prod
    where
        (table, prod) = mult x y