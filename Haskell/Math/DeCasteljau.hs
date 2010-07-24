module Math.DeCasteljau where

import Data.VectorSpace

interp a x y = lerp x y a

deCasteljau [] t = []
deCasteljau ps t = ps : deCasteljau (zipWith (interp t) ps (tail ps)) t

bezier ps = head . last . deCasteljau ps

split ps t = (map head pss, reverse (map last pss))
    where pss = deCasteljau ps t
